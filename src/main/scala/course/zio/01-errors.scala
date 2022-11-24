package course.zio

import course.zio.AppError.{CustomError, DatabaseError}
import zio._

import java.io.IOException

/*
 * INTRODUCTION
 *
 * ZIO effects model failure, in a way similar to the Scala data types `Try`
 * and `Either`. Unlike exceptions, ZIO effects are statically-typed, allowing
 * you to determine if and how effects fail by looking at type signatures.
 *
 * ZIO effects have a large number of error-related operators to transform
 * and combine effects. Some of these "catch" errors, while others transform
 * them, and still others combine potentially failing effects with fallback
 * effects.
 *
 * In this section, you will learn about all these operators, as well as the
 * rich underlying model of errors that ZIO uses internally.
 */

object ErrorConstructor extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `ZIO.fail`, construct an effect that models failure with any string
    * value, such as "Uh oh!". Explain the type signature of the effect.
    */
  val failed: ZIO[Any, String, Nothing] =
    ZIO.fail("Uh oh!")

  val run =
    failed
}

sealed trait AppError extends Throwable

object AppError {
  sealed trait DatabaseError extends AppError

  object DatabaseError {
    final case class NoSQLFound(message: String) extends DatabaseError
  }

  sealed trait CustomError extends AppError

  object CustomError {
    case object Whoopsie                   extends CustomError
    final case class Oops(message: String) extends CustomError
  }

}

object ErrorRecoveryOrElse extends ZIOAppDefault {

  final case class User()

  val getUsersSql: IO[DatabaseError.NoSQLFound, List[User]] =
    ZIO.fail(AppError.DatabaseError.NoSQLFound("WHere is the SQL?!"))

  val whoopsie: IO[CustomError.Whoopsie.type, String] = ZIO.fail(AppError.CustomError.Whoopsie)
  val oops: IO[CustomError.Oops, Int]                 = ZIO.fail(AppError.CustomError.Oops("Oopsie"))

  val program: ZIO[Any, DatabaseError.NoSQLFound, Int] =
    (whoopsie zip oops).as(1) orElse getUsersSql.as(2)

  /** EXERCISE
    *
    * Using `ZIO#orElse` have the `run` function compose the preceding `failed`
    * effect with another effect.
    */

  val run =
    program
}

object ErrorShortCircuit extends ZIOAppDefault {
  val program: ZIO[Any, Throwable, ExitCode] =
    for {
      _ <- Console.printLine("About to fail...")
      _ <- ZIO.fail(new Error("Uh oh!"))
      _ <- Console.printLine("This will NEVER be printed!")
    } yield ExitCode.success

  /** EXERCISE
    *
    * Using `ZIO#orElse`, compose the `failed` effect with another effect that
    * succeeds with an exit code.
    */
  val run =
    (program orElseSucceed ExitCode.failure).debug
}

object ErrorRecoveryFold extends ZIOAppDefault {

  val failed: IO[String, Int] = ZIO.fail("Uh oh!")

  private val value: UIO[Boolean] =
    failed.fold(
      string => string.length % 2 == 0,
      int => int > 1000
    )

  /** EXERCISE
    *
    * Using `ZIO#fold`, map both failure and success values of `failed` into the
    * unit value.
    */
  val run =
    value.debug
}

object ErrorRecoveryCatchAll extends ZIOAppDefault {

  val failed: ZIO[Any, String, Nothing] = ZIO.fail("Uh oh!")
  val value: ZIO[Any, Int, Nothing] =
    failed.mapError(_.length)

  /** EXERCISE
    *
    * Using `ZIO#catchAll`, catch all errors in `failed` and print them out to
    * the console using `Console.printLine`.
    */
  val run =
    value.debug("HELLO")
}

object ErrorRecoveryFoldZIO extends ZIOAppDefault {

  val coinToss: ZIO[Any, String, String] =
    Random.nextBoolean.flatMap {
      case true  => ZIO.succeed("Heads — You Win!")
      case false => ZIO.fail("Tails — You Lose!")
    }

  /** EXERCISE
    *
    * Using `ZIO#foldZIO`, print out the success or failure value of `coinToss`
    * by using `Console.printLine`.
    */
  val run =
    coinToss
      .foldZIO(
        string => Console.printLineError(s"OOPS: $string"),
        value => Console.printLine(s"YAY: $value")
      )
      .repeatN(100)

}

object ErrorRecoveryEither extends ZIOAppDefault {

  val coinToss: ZIO[Any, String, String] =
    Random.nextBoolean.flatMap {
      case true  => ZIO.succeed("Heads — You Win!")
      case false => ZIO.fail("Tails — You Lose!")
    }

  val manyTosses: ZIO[Any, Nothing, (Iterable[String], Iterable[String])] =
    ZIO.partition(1 to 10)(_ => coinToss)

  /** EXERCISE
    *
    * Using `ZIO#either`, surface the error of `failed` into the success
    * channel, and then map the `Either[String, Int]` into an exit code.
    */
  val run =
    manyTosses.debug("WHAT")
}

object ErrorRecoveryIgnore extends ZIOAppDefault {

  val failed: ZIO[Any, String, Nothing] = ZIO.fail("Uh oh!")

  /** EXERCISE
    *
    * Using `ZIO#ignore`, simply ignore the failure of `failed`.
    */
  val run =
    failed.ignoreLogged.debug("DO NOT CARE")
}

object RacingStuff extends ZIOAppDefault {

  def getWeather(city: String): ZIO[Any, String, RuntimeFlags] =
    for {
      _       <- ZIO.debug(s"Start getting weather for $city")
      secs    <- Random.nextIntBetween(2, 8)
      _       <- ZIO.sleep(secs.seconds)
      weather <- Random.nextIntBounded(100)
      _       <- ZIO.debug(s"Got weather for $city: $weather")
      _       <- ZIO.fail("WHOOPS").when(weather > 30)
    } yield weather

  val program = getWeather("nyc") race getWeather("chicago") race getWeather("boston")

  val run = program.debug
}

object OrDie extends ZIOAppDefault {

  val failed: ZIO[Any, Error, Nothing] = ZIO.fail(new Error("Uh oh!"))

  private val die: ZIO[Any, Nothing, Unit] = Console.printLine("hello").orDie

  /** EXERCISE
    *
    * Using `ZIO#orDie`, move the error into the "Die/Defect" channel.
    */
  val run: ZIO[Any, Nothing, Unit] = die
}

object ErrorRefinement1 extends ZIOAppDefault {
  import java.io.IOException

  /** EXERCISE
    *
    * Using `ZIO#refineToOrDie`, narrow the error type of `broadReadLine` into
    * an `IOException`:
    */
  val myReadLine: IO[IOException, String] =
    ZIO.attempt(scala.io.StdIn.readLine()).refineToOrDie[IOException]

  def myPrintLn(line: String): UIO[Unit] = ZIO.succeed(println(line))

  val run =
    for {
      _    <- myPrintLn("What is your name?")
      name <- myReadLine
      _    <- myPrintLn(s"Good to meet you, $name!")
    } yield ()
}

object ErrorRefinement2 extends ZIOAppDefault {

  import java.io.IOException

  /** EXERCISE
    *
    * Create an effect that will get a `Duration` from the user, by prompting
    * the user to enter a number of seconds. Use `refineToOrDie` to narrow the
    * error type as necessary.
    */
  lazy val getAlarmDuration: ZIO[Any, IOException, Duration] = {

    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.attempt(input.toLong).map(Duration.fromSeconds).refineToOrDie[NumberFormatException]

    def fallback(input: String): ZIO[Any, IOException, Duration] =
      Console.printLine(s"The input $input is not valid.") *> getAlarmDuration

    for {
      _        <- Console.printLine("Please enter the number of seconds to sleep: ")
      input    <- Console.readLine
      duration <- parseDuration(input) orElse fallback(input)
    } yield duration
  }

  /** EXERCISE
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using `ZIO.sleep(d)`, and then
    * prints out a wakeup alarm message, like "Time to wakeup!!!".
    */
  val run =
    for {
      sleepDuration <- getAlarmDuration
      _             <- ZIO.sleep(sleepDuration)
      _             <- Console.printLine("Time to wakeup!!!")
    } yield ()
}

object ZIOFinally extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `ZIO#ensuring`, attach an effect to the `tickingBomb` effect, which
    * will be executed whether `tickingBomb` succeeds or fails. Print out a
    * message to the console saying "Executed".
    */
  lazy val tickingBomb2 =
    tickingBomb.ensuring(ZIO.die(new Error("FINALIZE")))

  /** EXERCISE
    *
    * See if you can break the guarantee of `ZIO#ensuring` so that "Executed" is
    * not printed out.
    */
  val tickingBomb =
    ZIO.succeed("YAY BOOM").delay(throw new Error("BOMB"))
//    ZIO.sleep(1.second) *> ZIO.fail("Boom!")

  val run =
    tickingBomb2.tapErrorCause { cause =>
      ZIO.debug(cause)
    }
}

object TapError extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `ZIO#tapError`, print out the error value of `boom` to the console.
    */
  lazy val program: IO[String, Int] =
    boom.tapError(string => ZIO.fail("Whoops"))

  val boom: IO[String, Int] =
    Random.nextIntBounded(1000).flatMap { n =>
      ZIO.fail(s"I hate the number $n!")
    }

  val run =
    program.ignore.exit
}

// ZIO[R, E, A]
// 1. Succeed with an A
// 2. Fail with an E
// 3. Die with Throwable
// 4. Interruption
object SequentialCause extends ZIOAppDefault {
  val failed1 = Cause.fail("Uh oh 1")
  val failed2 = Cause.fail("Uh oh 2")
  val failed3 = Cause.fail("Uh oh 3")

  /** EXERCISE
    *
    * Using `Cause.++`, form a sequential cause by composing `failed1` and
    * `failed2`.
    */
  lazy val composed = failed1 ++ failed2 ++ failed3

  /** EXERCISE
    *
    * Using `Cause.prettyPrint`, dump out `composed` to the console.
    */
  val run =
    ZIO.debug(composed.prettyPrint)
}

object ParalellCause extends ZIOAppDefault {

  val failed1 = Cause.fail("Uh oh 1")
  val failed2 = Cause.fail("Uh oh 2")

  /** EXERCISE
    *
    * Using `Cause.&&`, form a parallel cause by composing `failed1` and
    * `failed2`.
    */
  lazy val composed = failed1 && failed2

  /** EXERCISE
    *
    * Using `Cause.prettyPrint`, dump out `composed` to the console.
    */
  val run =
    ZIO.debug(composed)
}

object Sandbox extends ZIOAppDefault {

  val failed1    = ZIO.fail("Uh oh 1")
  val failed2    = ZIO.fail("Uh oh 2")
  val finalizer1 = ZIO.fail(new Exception("Finalizing 1!")).orDie
  val finalizer2 = ZIO.fail(new Exception("Finalizing 2!")).orDie

  val composed = ZIO.uninterruptible {
    (failed1 ensuring finalizer1).delay(1.second) zipPar (failed2 ensuring finalizer2)
  }

  /** EXERCISE
    *
    * Using `ZIO#sandbox`, sandbox the `composed` effect and print out the
    * resulting `Cause` value to the console using `Console.printLine`.
    */
  // .either IO[E, A] => IO[Nothing, Either[E, A]
  // .sandbox IO[E, A] => IO[Cause[E], A]
  val run =
    composed.sandbox.tapError { cause =>
      ZIO.debug(cause)
    }
//      .tapErrorCause { cause =>
//      ZIO.debug(cause)
//    }
}
