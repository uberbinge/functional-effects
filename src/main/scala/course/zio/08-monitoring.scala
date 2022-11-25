package course.zio

import zio._
import zio.metrics.{Metric, MetricState}
import zio.metrics.MetricKeyType.Counter
import zio.metrics.MetricState.Histogram

object SimpleLogging extends ZIOAppDefault {

  /** EXERCISE
    *
    * Add logging using `ZIO.log` around each update of the ref.
    */
  val program =
    for {
      ref <- Ref.make(0)
      _ <- ZIO.foreachParDiscard(0 to 4) { _ =>
             ref.update(_ + 1)
           }
      value <- ref.get
    } yield value

  /** EXERCISE
    *
    * Surround `program` in `LogLevel.Error` to change its log level.
    */
  val program2: ZIO[Any, Nothing, Int] =
    LogLevel.Error(program)

  val run = program *> program2

}

object LogSpan extends ZIOAppDefault {

  final case class User(id: Int, name: String, passHash: String)

  /** EXERCISE
    *
    * Add a log span of "createUser" to the whole function.
    */
  def createUser(userName: String, passHash: String): ZIO[Any, Nothing, User] =
    for {
      _  <- ZIO.sleep(1.second)
      _  <- ZIO.log(s"Creating user $userName")
      id <- Random.nextIntBounded(100)
    } yield User(id, userName, passHash)

  /** EXERCISE
    *
    * Add a log span of "run" to the for comprehension
    */
  val run =
    for {
      _ <- ZIO.log(s"Starting App")
      _ <- ZIO.sleep(1.second)
      _ <- createUser("sherlockholmes", "jkdf67sf6")
    } yield ()
}

object CounterExample extends ZIOAppDefault {
  final case class Request(body: String)
  final case class Response(body: String)

  /** EXERCISE
    *
    * Use the constructors in `Metric` to make a counter metric that accepts
    * integers as input.
    */
  lazy val requestCounter =
    ???

  /** EXERCISE
    *
    * Use methods on the counter to increment the counter on every request.
    */
  def processRequest(request: Request): Task[Response] =
    ZIO.debug(s"Processing request: $request") *>
      ZIO.succeed(Response("OK"))

  /** EXERCISE
    *
    * Use methods on the counter to print out its value.
    *
    * NOTE: In real applications you don't need to poll metrics because they
    * will be exported to monitoring systems.
    */
  lazy val printCounter: ZIO[Any, Nothing, Unit] =
    ???

  lazy val run = {
    val processor = processRequest(Request("input")).repeat(Schedule.spaced(400.millis).jittered)
    val printer   = printCounter.schedule(Schedule.fixed(1.second))

    processor.race(printer)
  }
}
