package course.zio

import zio._

import java.util.UUID

object ForkJoin extends ZIOAppDefault {

  val printer =
    Console.printLine(".").repeatN(10)

  /** EXERCISE
    *
    * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then print
    * out a message, "Forked", then join the fiber using `Fiber#join`, and
    * finally, print out a message "Joined".
    */
  val run =
    printer
}

object ForkInterrupt extends ZIOAppDefault {

  val infinitePrinter =
    Console.print(".").delay(200.millis).forever

  /** EXERCISE
    *
    * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then print
    * out a message, "Forked", then using `ZIO.sleep`, sleep for 100
    * milliseconds, then interrupt the fiber using `Fiber#interrupt`, and
    * finally, print out a message "Interrupted".
    */
  val run =
    infinitePrinter *> ZIO.sleep(10.millis)
}

object ParallelFib extends ZIOAppDefault {

  /** EXERCISE
    *
    * Rewrite this implementation to compute nth fibonacci number in parallel.
    */
  def slowFib(n: Int): UIO[BigInt] = {
    def loop(n: Int, original: Int): UIO[BigInt] =
      if (n <= 1)
        ZIO.succeed(BigInt(n)).delay(1.second)
      else
        ZIO.debug(s"slowFib($n)") *>
          loop(n - 1, original).zipWith(loop(n - 2, original))(_ + _)

    loop(n, n)
  }

  val run =
    for {
      _ <- Console.printLine(
             "What number of the fibonacci sequence should we calculate?"
           )
      n <- Console.readLine.mapAttempt(_.toInt).eventually
      f <- slowFib(n)
      _ <- Console.printLine(s"fib($n) = $f")
    } yield ()
}

object TimeoutExample extends ZIOAppDefault {
  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n)
    else
      ZIO.suspendSucceed {
        fib(n - 1).zipWith(fib(n - 2))(_ + _)
      }

  /** EXERCISE
    *
    * Use `ZIO#timeout` to add a timeout to the following code so that it
    * doesn't run for more than 10 milliseconds.
    *
    * Print out a message if it timed out.
    */
  lazy val run = fib(20)
}

object RaceExample extends ZIOAppDefault {
  def loadFromCache: Task[String] =
    ZIO.succeed("Loaded from cache!").delay(1.second)

  def loadFromDB: Task[String] =
    ZIO.succeed("Loaded from DB!").delay(500.millis)

  /** EXERCISE
    *
    * Use `ZIO#race` to race the preceding two tasks and print out the winning
    * success value.
    */
  lazy val run = ???
}

object AlarmAppImproved extends ZIOAppDefault {

  import java.io.IOException
  import java.util.concurrent.TimeUnit

  lazy val getAlarmDuration: ZIO[Any, IOException, Duration] = {
    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO
        .attempt(
          Duration((input.toDouble * 1000.0).toLong, TimeUnit.MILLISECONDS)
        )
        .refineToOrDie[NumberFormatException]

    val fallback = Console.printLine("You didn't enter a number of seconds!") *> getAlarmDuration

    for {
      _        <- Console.printLine("Please enter the number of seconds to sleep: ")
      input    <- Console.readLine
      duration <- parseDuration(input) orElse fallback
    } yield duration
  }

  /** EXERCISE
    *
    * Create a program that asks the user for a number of seconds to sleep,
    * sleeps the specified number of seconds using ZIO.sleep(d), concurrently
    * prints a dot every second that the alarm is sleeping for, and then prints
    * out a wakeup alarm message, like "Time to wakeup!!!".
    */
  val run =
    ???
}

object ParallelZip extends ZIOAppDefault {

  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n)
    else
      ZIO.suspendSucceed {
        (fib(n - 1) zipWith fib(n - 2))(_ + _)
      }

  /** EXERCISE
    *
    * Compute fib(10) and fib(13) in parallel using `ZIO#zipPar`, and display
    * the result.
    */
  val run =
    ???
}

/** The Ref data type is a way for ZIO effects to utilize state. It is basically
  * a concurrent-safe version of Scala's own `var`, but integrated into ZIO.
  */
object RefExample extends ZIOAppDefault {
  import zio.Random._

  /** Some state to keep track of all points inside a circle, and total number
    * of points.
    */
  final case class PiState(
      inside: Ref[Long],
      total: Ref[Long]
  )

  /** A function to estimate pi.
    */
  def estimatePi(inside: Long, total: Long): Double =
    (inside.toDouble / total.toDouble) * 4.0

  /** A helper function that determines if a point lies in a circle of 1 radius.
    */
  def insideCircle(x: Double, y: Double): Boolean =
    Math.sqrt(x * x + y * y) <= 1.0

  /** An effect that computes a random (x, y) point.
    */
  val randomPoint: ZIO[Any, Nothing, (Double, Double)] =
    nextDouble zip nextDouble

  /** EXERCISE
    *
    * Using `Ref#update`, make a function that adds a point into the state. If
    * the point is inside the circle then increment `PiState#inside`. In any
    * case, increment `PiState#total`.
    */
  def addPoint(point: (Double, Double), piState: PiState): UIO[Unit] =
    ???

  /** EXERCISE
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  val run =
    ???
}

object InteractiveExerciseToolExample extends ZIOAppDefault {

  final case class UserId(value: UUID)      extends AnyVal
  final case class Submission(code: String) extends AnyVal

  final case class State(
      submissions: Map[UserId, Submission]
  ) {
    def submit(userId: UserId, submission: Submission): State =
      copy(submissions = submissions + (userId -> submission))

    def size: Int = submissions.size
  }

  object State {
    val empty = State(Map.empty)
  }

  trait SubmissionService {
    def addSubmission(userId: UserId, submission: Submission): UIO[Unit]
    def clearAll: UIO[Unit]
    def submissionCount: UIO[Int]
  }

  final case class SubmissionServiceVar() extends SubmissionService {
    var state = State.empty

    def addSubmission(userId: UserId, submission: Submission): UIO[Unit] =
      ZIO.succeed {
        state = state.submit(userId, submission)
      }

    def clearAll: UIO[Unit] =
      ZIO.succeed {
        state = State.empty
      }

    def submissionCount: UIO[Int] =
      ZIO.succeed {
        state.size
      }
  }

  final case class SubmissionServiceLive(ref: Ref[State]) {
    def addSubmission(userId: UserId, submission: Submission): UIO[Unit] =
      ???

    def clearAll: UIO[Unit] =
      ???

    def submissionCount: UIO[Int] =
      ???
  }

  val exampleCode =
    Vector("foo.bar", "println(\"hello world!\")", "ZIO.succeed(42)", "???")

  def randomUserSubmission: UIO[(UserId, Submission)] =
    for {
      userId <- Random.nextUUID.map(UserId)
      code   <- Random.nextIntBounded(exampleCode.size).map(exampleCode)
    } yield (userId, Submission(code))

  val run =
    ???
}

object PromiseExample extends ZIOAppDefault {

  /** EXERCISE
    *
    * Do some computation that produces an integer. When you're done, complete
    * the promise with `Promise#succeed`.
    */
  def doCompute(result: Promise[Nothing, Int]): UIO[Unit] = ???

  /** EXERCISE
    *
    * Fork the above computation in a separate fiber, giving it a promise that
    * it can use, and then wait for the promise to be completed, using
    * `Promise#await`.
    */
  lazy val waitForCompute: ZIO[Any, Nothing, Unit] = ???

  val run =
    waitForCompute
}

object FiberRefExample extends ZIOAppDefault {

  /** EXERCISE
    *
    * Make the child increment the ref and see how the output of the program
    * changes.
    */
  def makeChild(ref: FiberRef[Int]) =
    for {
      _ <- ref.get.debug("child initial value")
      _ <- ref.update(_ + 1)
      _ <- ref.get.debug("child after update")
    } yield ()

  val run =
    for {
      ref   <- FiberRef.make[Int](0, identity(_), _ + _)
      _     <- ref.get.debug("parent before fork")
      child <- makeChild(ref).fork
      _     <- ref.get.debug("parent after fork")
      _     <- child.join
      _     <- ref.get.debug("parent after join")
    } yield ()
}

object CustomLoggerWithSpans extends ZIOAppDefault {
  val run = ???
}
