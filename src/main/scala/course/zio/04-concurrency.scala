package course.zio

import zio.{RuntimeFlags => _, _}

import java.io.IOException
import java.util.UUID

// - Work Stealing
// - How is Dispatching
object ForkJoin extends ZIOAppDefault {

  val printer: ZIO[Any, IOException, String] =
    ZIO.uninterruptible {
      Console.print(".").delay(200.millis).repeatN(10).as("Done")
    }

  /** EXERCISE
    *
    * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then print
    * out a message, "Forked", then join the fiber using `Fiber#join`, and
    * finally, print out a message "Joined".
    */
  // 1
  // main fiber = printer -- - - -- - - - - > debug
  //
  // 2
  // main fiber = printer.fork -> debug -> await
  // side fiber =             |           . . . . . . . . . . . . .
  // TODO: Explore more what yield does w/r/t the fiber we're running on
  val run =
    for {
      _ <- ZIO.fiberId.debug
      _ <- ZIO.fiberId.debug.fork
      _ <- ZIO.sleep(1.second)
    } yield ()
}

object ForkInterrupt extends ZIOAppDefault {

  val infinitePrinter =
    Console.print(".").delay(200.millis).forever

  private val print2Seconds: ZIO[Any, Nothing, Unit] = for {
    _ <- infinitePrinter.forkDaemon
    _ <- ZIO.sleep(2.seconds)
  } yield ()

  /** EXERCISE
    *
    * Using `ZIO#fork`, fork the `printer` into a separate fiber, and then print
    * out a message, "Forked", then using `ZIO.sleep`, sleep for 100
    * milliseconds, then interrupt the fiber using `Fiber#interrupt`, and
    * finally, print out a message "Interrupted".
    */

  // TODO: Why? Does forkDaemon get interrupted? Does it?
  val run =
    for {
      _ <- ZIO.uninterruptible(print2Seconds).fork
      _ <- ZIO.sleep(500.millis)
//      _ <- ZIO.sleep(2.seconds) _ <- ZIO.debug("It should stop now")
//      _ <- ZIO.sleep(2.seconds)
//      _ <- ZIO.debug("Did it stop?")
    } yield ()
}

object ParallelFib extends ZIOAppDefault {

  /** EXERCISE
    *
    * Rewrite this implementation to compute nth fibonacci number in parallel.
    */

  val printHello = ZIO.succeed(println("Hello"))

  def slowFib(n: Int): UIO[BigInt] = {
    def loop(n: Int, original: Int): UIO[BigInt] =
      if (n <= 1)
        ZIO.succeed(BigInt(n)).delay(1.second)
      else
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
    if (n <= 1)
      ZIO.succeed(n)
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
  lazy val run = fib(30).timeout(1000.millis).someOrElse(999).debug
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
  lazy val run = (loadFromCache.onInterrupt(ZIO.debug("OOPS")) race loadFromDB).debug
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
    for {
      duration <- getAlarmDuration
      _        <- Console.printLine(".").delay(1.second).forever.fork
      _        <- ZIO.sleep(duration)
      _        <- Console.printLine("Time to wakeup!!!")
    } yield ()
}

final case class Config()
final case class Database()

final case class Service(config: Config, db: Database, ref: Ref[Int])

object Service {
  val layer: ZLayer[Database with Config, Nothing, Service] =
    ZLayer(Ref.make(10)) >>>
      ZLayer.fromFunction(Service.apply _)
}

object ParallelZip extends ZIOAppDefault {

  def fib(n: Int): UIO[Int] =
    if (n <= 1) ZIO.succeed(n).delay(1.second)
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
    (fib(3).debug("DONE ONE") zipPar fib(3).debug("DONE TWO")).debug
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
  def addPoint(point: (Double, Double), piState: PiState): UIO[Unit] = {
    val (x, y) = point
    for {
      _ <- piState.total.update(_ + 1)
      _ <- piState.inside.update(_ + 1).when(insideCircle(x, y))
    } yield ()
  }

  def shootDartsForever(state: PiState): UIO[Unit] =
    (for {
      p <- randomPoint
      _ <- addPoint(p, state)
      _ <- ZIO.fiberId.debug("HEY!")
      _ <- ZIO.sleep(1.second)
    } yield ()).forever
      .onInterrupt(ZIO.debug("YOU KILLED ME"))

  def printEstimate(state: PiState): UIO[Unit] =
    (for {
      inside <- state.inside.get
      total  <- state.total.get
      _      <- ZIO.debug(s"ESTIMATE: ${estimatePi(inside, total)}")
      _      <- ZIO.sleep(500.millis)
    } yield ()).forever

  /** EXERCISE
    *
    * Build a multi-fiber program that estimates the value of `pi`. Print out
    * ongoing estimates continuously until the estimation is complete.
    */
  val run = for {
    insideRef <- Ref.make(0L)
    totalRef  <- Ref.make(0L)
    state      = PiState(insideRef, totalRef)
    fiber     <- ZIO.forkAll(List.fill(4)(shootDartsForever(state)))
    _         <- fiber.interrupt
    _         <- printEstimate(state).fork
    _         <- Console.readLine("Enter to exit estimation")
    inside    <- insideRef.get
    total     <- totalRef.get
    _         <- Console.printLine(s"Estimate for PI: ${estimatePi(inside, total)}")
  } yield ()
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
    val empty: State = State(Map.empty)
  }

  trait SubmissionService {
    def addSubmission(userId: UserId, submission: Submission): UIO[Unit]
    def clearAll: UIO[Unit]
    def submissionCount: UIO[Int]
  }

  final case class SubmissionServiceVar() extends SubmissionService {
    var state: State = State.empty

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

  final case class SubmissionServiceLive(ref: Ref[State]) extends SubmissionService {
    def addSubmission(userId: UserId, submission: Submission): UIO[Unit] =
      ref.update(_.submit(userId, submission))

    def clearAll: UIO[Unit] =
      ref.set(State.empty)

    def submissionCount: UIO[Int] =
      ref.get.map(_.size)
  }

  object SubmissionServiceLive {
    val make: ZIO[Any, Nothing, SubmissionService] =
      Ref.make(State.empty).map(SubmissionServiceLive(_))
  }

  val exampleCode =
    Vector("foo.bar", "println(\"hello world!\")", "ZIO.succeed(42)", "???")

  def randomUserSubmission: UIO[(UserId, Submission)] =
    for {
      userId <- Random.nextUUID.map(UserId)
      code   <- Random.nextIntBounded(exampleCode.size).map(exampleCode)
    } yield (userId, Submission(code))

  val run =
    for {
      service <- SubmissionServiceLive.make
      submit = randomUserSubmission.flatMap { //
                 case (uid, submission) => service.addSubmission(uid, submission)
               }
      _ <- ZIO.foreachParDiscard(1 to 100_000)(_ => submit)
      _ <- service.submissionCount.debug
    } yield ()
}

object PromiseExample extends ZIOAppDefault {

  val readInt: ZIO[Any, Nothing, Int] = Console
    .readLine("Gimme number")
    .orDie
    .map(_.toInt)

  /** EXERCISE
    *
    * Do some computation that produces an integer. When you're done, complete
    * the promise with `Promise#succeed`.
    */
  def doCompute(promise: Promise[String, Int]): UIO[Unit] =
    readInt.flatMap { int =>
      if (int == 563) promise.fail("I hate this number")
      else promise.succeed(int)
    }.unit

  /** EXERCISE
    *
    * Fork the above computation in a separate fiber, giving it a promise that
    * it can use, and then wait for the promise to be completed, using
    * `Promise#await`.
    */
  // TODO: Can I name my fibers?
  lazy val waitForCompute: ZIO[Any, String, Unit] =
    for {
      promise <- Promise.make[String, Int]
      _       <- doCompute(promise).fork
      _       <- ZIO.debug(s"HRMM")
      int     <- promise.await
      _       <- ZIO.debug(s"I GOT AN INT: $int")
    } yield ()

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
      ref   <- FiberRef.make[Int](0, fork = _ + 100, join = _ + _)
      _     <- ref.get.debug("parent before fork")
      child <- makeChild(ref).fork
      _     <- ZIO.sleep(1.second)
      _     <- ref.get.debug("parent after fork")
      _     <- ref.update(_ + 5)
      _     <- ref.get.debug("parent before join")
      // TODO: how to get fiber ref value from interrupted children
      _ <- child.join.catchAllCause(_ => ZIO.debug("WHOOS"))
      _ <- ref.get.debug("parent after join")
    } yield ()
}

trait Logger {
  def log(message: String): UIO[Unit]
  def annotate[R, E, A](key: String, value: String)(zio: ZIO[R, E, A]): ZIO[R, E, A]
}

final case class LoggerLive(ref: FiberRef[Map[String, String]]) extends Logger {
  override def log(message: String): UIO[Unit] =
    for {
      map    <- ref.get
      context = map.mkString(",")
      _      <- Console.printLine(s"[$context] $message").orDie
    } yield ()

  override def annotate[R, E, A](key: String, value: String)(zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ref.locallyWith(_.updated(key, value))(zio)
}

object LoggerLive {
  val layer: ULayer[Logger] = ZLayer.scoped {
    FiberRef.make(Map.empty[String, String]).map(LoggerLive(_))
  }
}

object Logger {
  def log(message: String): ZIO[Logger, Nothing, Unit] =
    ZIO.serviceWithZIO[Logger](_.log(message))

  def annotate[R, E, A](key: String, value: String)(zio: ZIO[R, E, A]): ZIO[R with Logger, E, A] =
    ZIO.serviceWithZIO[Logger](_.annotate(key, value)(zio))
}

object CustomLoggerWithSpans extends ZIOAppDefault {
  def otherOp =
    for {
      _ <- Logger.log("OTHER")
    } yield ()

  def someImportantOperation =
    for {
      _   <- ZIO.debug("I am very important")
      _   <- Logger.log("Look at me be important over here please")
      int <- Random.nextIntBounded(10)
      _   <- Logger.annotate("another thing", int.toString)(otherOp)
      _   <- Logger.log("Look at me be important over here please")
    } yield ()

  val program =
    for {
      _ <- Logger.log("I am logging")
      _ <- Logger.annotate("important?", "YES")(someImportantOperation) zipPar
             Logger.annotate("important?", "NO")(someImportantOperation) zipPar
             Logger.annotate("important?", "Maybe")(someImportantOperation) zipPar
             Logger.annotate("cool?", "sure")(someImportantOperation)
      _ <- Logger.log("I am done")
    } yield ()

  val run = program.provide(LoggerLive.layer)

}
