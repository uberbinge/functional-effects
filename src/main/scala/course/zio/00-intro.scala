package course.zio

import zio._

import java.io.IOException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Try

// Program that returns some type A
final case class Effect[A](program: () => A) {
  def run: A = program()

  def map[B](f: A => B): Effect[B] =
    Effect(() => f(this.program()))

}

object Effect extends App {
  def printLine(string: String): Unit = println(string)

  val readLine: Effect[String] = Effect(() => scala.io.StdIn.readLine())

  val printHelloDescription: Effect[Unit] =
    Effect(() => printLine("Hello"))

  val effect: Effect[Int] = readLine.map { string =>
    string.toInt + 1000
  }

  println(effect.run)
}

/*
 * INTRODUCTION
 *
 * ZIO effects are immutable data values that model a possibly complex series
 * of async, concurrent, resourceful, and contextual computations.
 *
 * The only effect type in ZIO is called ZIO, and has three type parameters,
 * which permit accessing context from an environment (`R`), failing with a
 * value of a certain type (`E`), and succeeding with a value of a certain
 * type (`A`).
 *
 * Unlike Scala's Future, ZIO effects are completely lazy. All methods on ZIO
 * effects return new ZIO effects. No part of the workflow is executed until
 * one of the `unsafeRun*` functions are called.
 *
 * ZIO effects are transformed and combined using methods on the ZIO data type.
 * For example, two effects can be combined into a sequential workflow using
 * an operator called `zip`. Similarly, two effects can be combined into a
 * parallel workflow using an operator called `zipPar`.
 *
 * The operators on the ZIO data type allow very powerful, expressive, and
 * type-safe transformation and composition, while the methods in the ZIO
 * companion object allow building new effects from simple values (which are
 * not themselves effects).
 *
 * In this section, you will explore both the ZIO data model itself, as well
 * as the very basic operators used to transform and combine ZIO effects, as
 * well as a few simple ways to build effects.
 */

/** A good mental model for ZIO[R, E, A] is:
  * {{{
  *   ZEnvironment[R] => Either[E, A]
  * }}}
  * This can be interpreted as a function which, given a ZIO environment (which
  * is a map that contain classes of different types), a ZIO returns either a
  * failure of type E or a success of type A.
  */

object SillyExamples extends App {
  def ifThenElse[A](cond: Boolean, ifTrue: => A, ifFalse: => A) =
    if (cond) ifTrue else ifFalse

  ifThenElse(
    cond = true,
    ifTrue = println("ITS TRUE"),
    ifFalse = println("NO ITS NOT")
  )
}

object ZEnvExample extends ZIOAppDefault {

  val env1: ZEnvironment[Int] =
    ZEnvironment(123)
  val env2: ZEnvironment[String] =
    ZEnvironment("Hello")
  val env3: ZEnvironment[Int with String with Boolean] =
    env1 ++ env2 ++ ZEnvironment(true) ++ ZEnvironment("cool")
  println(env3)

  // String >: String with Int
  // Double >: String with Int
//  env3.get[String with Int]

  final case class LazyString(makString: () => String) {
    lazy val string: String = makString()
  }

  // Map("Int" -> 123)
  // env3 = Map("Int" -> 123, "String" -> "Hello")
  env3.get[Boolean]

  val example1: ZIO[Int, Any, Int] =
    ZIO.service[Int].map(_ + 1000)

  val example2: ZIO[String, Any, String] =
    ZIO.service[String].map(_.toUpperCase)

  // "phantom" type parameter
  // type-level set of required values
  val example3: ZIO[Int & String, Any, (Int, String)] =
    example1 zip example2

  val run =
    example3
      .provide(
        ZLayer.succeed("hello"),
        ZLayer.succeed(123)
      )
      .debug
}

object ZIOModel {

  /** EXERCISE
    *
    * Implement all missing methods on the ZIO companion object.
    */

  // 1. by-name parameters
  // 2. zenvironment ???
  object ZIO {
    def succeed[A](success: => A): ZIO[Any, Nothing, A] =
      ZIO[Any, Nothing, A] { _ =>
        Right(success)
      }

    def fail[E](error: => E): ZIO[Any, E, Nothing] =
      ZIO { _ =>
        Left(error)
      }

    def attempt[A](code: => A): ZIO[Any, Throwable, A] =
      ZIO { _ =>
        try Right(code)
        catch {
          case t: Throwable => Left(t)
        }
      }

    def environment[R]: ZIO[R, Nothing, ZEnvironment[R]] =
      ZIO { env =>
        Right(env)
      }
  }

  /** EXERCISE
    *
    * Implement all missing methods on the ZIO class.
    */
  final case class ZIO[-R, +E, +A](run: ZEnvironment[R] => Either[E, A]) { self =>
    def map[B](f: A => B): ZIO[R, E, B] =
      flatMap(a => ZIO.succeed(f(a)))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO { env =>
        run(env) match {
          case Left(value) => Left(value)
          case Right(a)    => f(a).run(env)
        }
      }

    def zip[R1 <: R, E1 >: E, B](that: ZIO[R1, E1, B]): ZIO[R1, E1, (A, B)] =
      for {
        a <- this
        b <- that
      } yield (a, b)

    def either: ZIO[R, Nothing, Either[E, A]] =
      ZIO { env =>
        Right(run(env))
      }

    def provide(r: ZEnvironment[R]): ZIO[Any, E, A] =
      ZIO { _ =>
        run(r)
      }

    def orDie(implicit ev: E <:< Throwable): ZIO[R, Nothing, A] =
      ZIO(r => self.run(r).fold(throw _, Right(_)))
  }

  def printLine(line: String): ZIO[Any, Nothing, Unit] = {
    ZIO.fail("123") // ZIO[Any, String, Nothing]
    ZIO.succeed(println(line))
  }

  val readLine: ZIO[Any, Nothing, String] =
    ZIO.attempt(scala.io.StdIn.readLine()).orDie

  def run[A](zio: ZIO[Any, Throwable, A])(implicit unsafe: Unsafe): A =
    zio.run(ZEnvironment.empty).fold(throw _, a => a)

  /** Run the following main function and compare the results with your
    * expectations.
    */
  def main(args: Array[String]): Unit =
    Unsafe.unsafe { implicit u =>
      run {
        val effect: ZIO[Int, Nothing, Unit] = for {
          int  <- ZIO.environment[Int]
          _    <- printLine(s"Hello, what is your name? (env: $int)")
          name <- readLine
          _    <- printLine(s"Your name is: $name")
        } yield ()
        effect.provide(ZEnvironment(123))
      }
    }
}

object ZIOTypes {
  type ??? = Nothing

  // Any == No Environment

  /** EXERCISE
    *
    * Provide definitions for the ZIO type aliases below.
    */
  type Task[+A]     = ZIO[Any, Throwable, A]
  type UIO[+A]      = ZIO[Any, Nothing, A]
  type IO[+E, +A]   = ZIO[Any, E, A]
  type RIO[-R, +A]  = ZIO[R, Throwable, A]
  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object SuccessEffect extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `ZIO.succeed`, create an effect that succeeds with the string "Hello
    * World".
    */
  val run =
    ZIO.succeed("Hello")
}

object HelloWorld extends ZIOAppDefault {

  /** EXERCISE
    *
    * Implement a simple "Hello World!" program by invoking `Console.printLine`
    * to create an effect that, when executed, will print out "Hello World!" to
    * the console.
    */
  val run =
    Console.printLine("Hello").orDie
}

object SimpleMap extends ZIOAppDefault {
  import Console.readLine

  /** EXERCISE
    *
    * Using `ZIO#map`, map the string success value of `Console.readLine` into
    * an integer (the length of the string)`.
    */
  val run =
    Console.readLine.map(_.length).debug
}

object PrintSequenceZip extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `zip`, compose a sequence of `Console.printLine` effects to produce
    * an effect that prints three lines of text to the console.
    */
  val run =
    (Console.printLine("Hello") zip ZIO.succeed(true) zip ZIO.succeed("hello")).debug
}

object PrintSequence extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `*>` (`zipRight`), compose a sequence of `Console.printLine` effects
    * to produce an effect that prints three lines of text to the console.
    */
  val run =
    Console.printLine("Hello") *>
      Console.printLine("World") *>
      Console.printLine("!!!")
}

object PrintReadSequence extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `*>` (`zipRight`), sequentially compose a `Console.printLine`
    * effect, which models printing out "Hit Enter to exit...", together with a
    * `Console.readLine` effect, which models reading a line of text from the
    * console.
    */
  val run =
    Console.printLine("Hit Enter to exit...") *> Console.readLine
}

object SimpleDuplication extends ZIOAppDefault {

  //    Console.printLine("Hello again")

  val program: Task[Unit] = ZIO.fromFuture { _ =>
    Future(println("hello"))
  }

  // https://scala-class.herokuapp.com

  val run =
    for {
      _ <- program
      _ <- program
      _ <- program
      _ <- program
    } yield ()
}

object FlatMap extends ZIOAppDefault {

  /** EXERCISE
    *
    * The following program is intended to ask the user for their name, then
    * read their name, then print their name back out to the user. However, the
    * `zipRight` (`*>`) operator is not powerful enough to solve this problem,
    * because it does not allow a _subsequent_ effect to depend on the success
    * value produced by a _preceding_ effect.
    *
    * Solve this problem by using the `ZIO#flatMap` operator, which composes a
    * first effect together with a "callback", which can return a second effect
    * that depends on the success value produced by the first effect.
    */
  val run =
    Console.printLine("What is your name?") *>
      Console.readLine.flatMap { name =>
        Console.printLine(s"Your name is: $name")
      }
}

object PromptName extends ZIOAppDefault {

  /** EXERCISE
    *
    * The following program uses a combination of `zipRight` (`*>`), and
    * `flatMap`. However, this makes the structure of the program harder to
    * understand. Replace all `zipRight` by `flatMap`, by ignoring the success
    * value of the left hand effect.
    */
  val run =
    Console.printLine("What is your name?") *> Console.readLine.flatMap(name =>
      Console.printLine(s"Your name is: $name")
    )

  /** EXERCISE
    *
    * Implement a generic "zipRight" that sequentially composes the two effects
    * using `flatMap`, but which succeeds with the success value of the effect
    * on the right-hand side.
    */
  def myZipRight[R, E, A, B](
      left: ZIO[R, E, A],
      right: ZIO[R, E, B]
  ): ZIO[R, E, B] =
    // option-enter
    for {
      _ <- left
      r <- right
    } yield r
}

object ForComprehension extends ZIOAppDefault {

  val x = 123
  println("hello")
  val name = scala.io.StdIn.readLine()
  println(name + x)

  /** EXERCISE
    *
    * Rewrite the following program to use a `for` comprehension.
    */
  val run =
    Console
      .printLine("What is your name?")
      .flatMap(_ =>
        Console.readLine
          .flatMap(name =>
            Console
              .printLine(s"Your name is: $name")
              .map(_ => ())
          )
      )

}

object ForComprehensionBackward extends ZIOAppDefault {

  val readInt = Console.readLine.flatMap(string => ZIO.attempt(string.toInt)).orDie

  /** EXERCISE
    *
    * Rewrite the following program, which uses a `for` comprehension, to use
    * explicit `flatMap` and `map` methods. Note: each line of the `for`
    * comprehension will translate to a `flatMap`, except the final line, which
    * will translate to a `map`.
    */
  val run =
    for {
      _   <- Console.printLine("How old are you?")
      age <- readInt
      _ <- if (age < 18) Console.printLine("You are a kid!")
           else Console.printLine("You are all grown up!")
    } yield ()
}

object NumberGuesser extends ZIOAppDefault {
  def analyzeAnswer(random: Int, guess: String): IO[IOException, Unit] =
    if (random.toString == guess.trim) Console.printLine("You guessed correctly!")
    else Console.printLine(s"You did not guess correctly. The answer was $random")

  /** EXERCISE
    *
    * Choose a random number (using `Random.nextInt`), and then ask the user to
    * guess the number (using `Console.readLine`), feeding their response to
    * `analyzeAnswer`, above.
    */
  val run =
    for {
      secret <- Random.nextInt
      guess  <- Console.readLine("Your guess: ")
      _      <- analyzeAnswer(secret, guess)
    } yield ()
}

object SingleSyncInterop extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using ZIO.attempt, convert `println` into a ZIO function.
    */
  def myPrintLn(line: String): Task[Unit] =
    ZIO.attempt(println(line))

  val run =
    myPrintLn("Hello World!")
}

object MultipleSyncInterop extends ZIOAppDefault {

  /** Using `ZIO.attempt`, wrap Scala's `println` method to lazily convert it
    * into a functional effect, which describes the action of printing a line of
    * text to the console, but which does not actually perform the print.
    */
  def printLine(line: String): Task[Unit] =
    ZIO.attempt(println(line))

  /** Using `ZIO.attempt`, wrap Scala's `scala.io.StdIn.readLine()` method to
    * lazily convert it into a ZIO effect, which describes the action of
    * printing a line of text to the console, but which does not actually
    * perform the print.
    */
  val readLine: Task[String] =
    ZIO.attempt(scala.io.StdIn.readLine())

  val run =
    for {
      _    <- printLine("Hello, what is your name?")
      name <- readLine
      _    <- printLine(s"Good to meet you, $name!")
    } yield ()
}

object AsyncExample extends ZIOAppDefault {
  import scala.concurrent.ExecutionContext.global

  def loadBodyAsync(onSuccess: String => Unit, onFailure: Throwable => Unit): Unit =
    global.execute { () =>
      if (scala.util.Random.nextDouble() < 0.5) onFailure(new java.io.IOException("Could not load body!"))
      else onSuccess("Body of request")
    }

  /** EXERCISE
    *
    * Using `ZIO.async`, convert the above callback-based API into a nice clean
    * ZIO effect.
    */
  lazy val loadBodyAsyncZIO: ZIO[Any, Throwable, String] =
    ZIO.async { complete =>
      loadBodyAsync(
        onSuccess = string => complete(ZIO.succeed(string)),
        onFailure = err => complete(ZIO.fail(err))
      )
    }

  val run =
    for {
      body <- loadBodyAsyncZIO
      _    <- Console.printLine(body)
    } yield ()
}
