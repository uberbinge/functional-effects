package course.zio

import zio._

import java.time.Instant

object RecursRepeat extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.recurs`, create a schedule that recurs 5 times.
    */
  val fiveTimes: Schedule[Any, Any, Long] =
    ???

  /** EXERCISE
    *
    * Using the `ZIO.repeat`, repeat printing "Hello World" five times to the
    * console.
    */
  val run =
    ???
}

object RecursRetry extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.recurs`, create a schedule that recurs 5 times.
    */
  val fiveTimes: Schedule[Any, Any, Long] =
    ???

  /** EXERCISE
    *
    * Using the `ZIO.retry`, retry the failing effect with the `fiveTimes`
    * Schedule.
    */
  val failing = ZIO.debug("Let's Fail!") *> ZIO.fail("Failed")

  val run =
    ???
}

object Spaced extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.spaced`, create a schedule that recurs forever every 1
    * second.
    */
  val everySecond = ???

  val run =
    Console.printLine("Hello World").repeat(everySecond)

}

object BothSchedules extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using the `&&` method of the `Schedule` object, `recurs`, and `spaced` to
    * create a schedule that recurs every second, five times.
    */
  val everySecondFiveTimes =
    ???

  /** EXERCISE
    *
    * Using the `ZIO#repeat`, repeat the action Console.printLine("Hi hi") using
    * `everySecondFiveTimes`.
    */
  val run =
    Console.printLine("Hi hi").repeat(everySecondFiveTimes)
}

object AndThenSchedules extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule#andThen` the `fiveTimes` schedule, and the `everySecond`
    * schedule, create a schedule that repeats fives times rapidly, and then
    * repeats every second forever.
    */
  val fiveTimesThenEverySecond =
    ???

  /** EXERCISE
    *
    * Using `ZIO#retry`, retry the following error a total of five times.
    */
  val error1 =
    ZIO.debug("ABOUT TO FAIL") *>
      ZIO.fail("Uh oh!")

  val run = error1.retry(fiveTimesThenEverySecond)

}

object OrElseSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using the `Schedule#||`, the `fiveTimes` schedule, and the `everySecond`
    * schedule, create a schedule that repeats the minimum of five times and
    * every second.
    */
  val fiveTimesOrEverySecond =
    ???

  val run =
    Console.printLine("Okay").repeat(fiveTimesOrEverySecond)
}

object ExponentialSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.exponential`, create an exponential schedule that starts
    * from 10 milliseconds.
    */
  val exponentialSchedule = ???

  val failedWebRequest = {
    Random.nextIntBounded(10000).debug("Web Request About To Fail!") *>
      ZIO.fail("Uh oh!")
  }

  val run =
    failedWebRequest.retry(exponentialSchedule)
}

object JitteredSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.jittered` produced a jittered version of a `spaced`
    * schedule.
    */
  val jitteredExponential = ???

  val startTime     = Instant.now()
  def elapsedMillis = Instant.now().toEpochMilli - startTime.toEpochMilli

  val fail = ZIO.debug(s"FAILING $elapsedMillis") *> ZIO.fail("Uh oh!")

  val run = fail.retry(jitteredExponential)

}

object WhileOutputSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.whileOutput`, produce a filtered schedule from
    * `Schedule.forever` that will halt when the number of recurrences exceeds
    * 100.
    */
  val oneHundred = ???

  var n         = -1
  def increment = ZIO.succeed { n += 1; n }.debug("n")

  val run = increment.repeat(oneHundred)

}

object IdentitySchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule.identity`, produce a schedule that recurs forever, without
    * delay, returning its inputs.
    */
  def identitySchedule[A]: Schedule[Any, A, A] =
    ???

  val run =
    Random.nextIntBounded(1000).debug.repeat(identitySchedule)
}

object CollectSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `Schedule#collectAll`, produce a schedule that recurs forever,
    * collecting its inputs into a chunk.
    */
  def collectedInputs[A]: Schedule[Any, A, Chunk[A]] =
    ???

  val run =
    Random.nextIntBounded(1000).debug.repeat(collectedInputs)

}

object ZipRightSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `*>` (`zipRight`), combine `fiveTimes` and `everySecond` but return
    * the output of `everySecond`.
    */
  val fiveTimesEverySecondR =
    ???

  val run = Console.printLine("Hi hi").repeat(fiveTimesEverySecondR).debug
}

object FinalSchedule extends ZIOAppDefault {

  /** EXERCISE
    *
    * Produce a jittered schedule that first does exponential spacing (starting
    * from 10 milliseconds), but then after the spacing reaches 60 seconds,
    * switches over to fixed spacing of 60 seconds between recurrences, but will
    * only do that for up to 100 times, and produce a list of the inputs to the
    * schedule.
    */
  import Schedule.{collectAll, exponential, fixed, recurs}
  def mySchedule[A]: Schedule[Any, A, List[A]] = ???

  val run = ???
}
