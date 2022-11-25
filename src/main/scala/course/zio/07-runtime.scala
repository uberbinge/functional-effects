package course.zio

import zio._

import scala.concurrent.ExecutionContext
import java.io.IOException

object CustomRuntime {
  final case class AppConfig(name: String)

  val defaultEnvironment  = ZEnvironment.empty
  val defaultRuntimeFlags = RuntimeFlags.default
  val defaultFiberRefs    = FiberRefs.empty

  /** EXERCISE
    *
    * Create a custom runtime that bundles a value of type `AppConfig` into the
    * environment.
    */
  lazy val customRuntime: Runtime[Any] =
    Runtime(defaultEnvironment, defaultFiberRefs, defaultRuntimeFlags)

  val program: ZIO[AppConfig, IOException, Unit] =
    for {
      appConfig <- ZIO.service[AppConfig]
      _         <- Console.printLine(s"Application name is ${appConfig.name}")
      _         <- Console.printLine("What is your name?")
      name      <- Console.readLine
      _         <- Console.printLine(s"Hello, $name!")
    } yield ()

  /** EXERCISE
    *
    * Using the `unsafe.run` method of the custom runtime you created, execute
    * the `program` effect above.
    *
    * NOTE: You will have to use `Unsafe.unsafe { implicit u => ... }` or
    * `Unsafe.unsafe { ... }` (Scala 3) in order to call `run`.
    */
  def main(args: Array[String]): Unit =
    ???
}

object ThreadPool extends ZIOAppDefault {

  lazy val dbPool: Executor = Executor.fromExecutionContext(ExecutionContext.global)

  /** EXERCISE
    *
    * Using `ZIO#onExecutor`, write an `onDatabase` combinator that runs the
    * specified effect on the database thread pool.
    */
  def onDatabase[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    ???

  /** EXERCISE
    *
    * Implement a combinator to print out thread information before and after
    * executing the specified effect.
    */
  def threadLogged[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    val log = ZIO.succeed {
      val thread = Thread.currentThread()

      val id        = ???
      val name      = ???
      val groupName = ???

      println(s"Thread($id, $name, $groupName)")
    }

    zio
  }

  /** EXERCISE
    *
    * Use the `threadLogged` combinator around different effects below to
    * determine which threads are executing which effects.
    */
  val run =
    ZIO.debug("Main") *>
      onDatabase {
        ZIO.debug("Database") *>
          ZIO.blocking {
            ZIO.debug("Blocking")
          } *>
          ZIO.debug("Database")
      } *>
      ZIO.debug("Main")
}

object CustomLogger {

  /** EXERCISE
    *
    * Using `ZLogger.simple`, create a logger that dumps text strings to the
    * console using `println`.
    */
  lazy val simpleLogger: ZLogger[String, Unit] = ???

  /** EXERCISE
    *
    * Create a layer that will install your simple logger using
    * Runtime.addLogger.
    */
  lazy val withCustomLogger: ZLayer[Any, Nothing, Unit] = ???

  /** EXERCISE
    *
    * Using `ZIO#provide`, inject the custom logger into the following effect
    * and verify your logger is being used.
    */
  val run =
    ZIO.log("Hello World!")
}

// ALSO: https://github.com/mlangc/zio-interop-log4j2
//
//object MDCInterop {
//  def withLoggingContext[A](eff: => A): ZIO[Any, Throwable, A] =
//    ZIO.logAnnotations.flatMap { ctx =>
//      ZIO.attempt {
//        import scala.jdk.CollectionConverters._
//        val previous =
//          Option(MDC.getCopyOfContextMap().asScala)
//            .getOrElse(Map.empty[String, String])
//
//        try {
//          ctx.renderContext.foreach((MDC.put _).tupled)
//          eff
//        } finally MDC.setContextMap(previous.asJava)
//      }
//    }
//}
