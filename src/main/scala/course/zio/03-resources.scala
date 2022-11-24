package course.zio

import zio._
import java.text.NumberFormat
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.io.Source

object Cat extends ZIOAppDefault {

  import java.io.IOException

  def getScalaSource(file: String): Source = Source.fromFile(file)

  /** EXERCISE
    *
    * Using `ZIO.attemptBlocking`, implement a function to read a file on the
    * blocking thread pool, storing the result into a string. You may find it
    * helpful to use the `getScalaSource` helper method defined above.
    */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO
      .attemptBlockingIO {
        getScalaSource(file).mkString
      }

  /** EXERCISE
    *
    * Implement a version of the command-line utility "cat", which dumps the
    * contents of the specified file to standard output.
    */
  val run =
    for {
      args     <- getArgs
      contents <- readFile(args.head)
      _        <- Console.printLine(contents)
    } yield ()
}

object Object {
  def main(args: Array[String]): Unit =
    println(s"Got args: ${args.toList}")
}

object CatEnsuring extends ZIOAppDefault {

  import java.io.IOException
  import scala.io.Source

  def open(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(Source.fromFile(file))

  def close(source: Source): ZIO[Any, IOException, Unit] =
    ZIO.debug("CLOSING!") *>
      ZIO.attemptBlockingIO(source.close())

  /** EXERCISE
    *
    * Using `ZIO#ensuring`, implement a safe version of `readFile` that cannot
    * fail to close the file, no matter what happens during reading.
    */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO
      .uninterruptible {
        for {
          source   <- open(file)
          contents <- ZIO.attempt(source.mkString).ensuring(close(source).orDie)
        } yield contents
      }
      .refineToOrDie[IOException]

  val run =
    for {
      args <- getArgs
      fileName <- ZIO
                    .fromOption(args.headOption)
                    .tapError(_ => Console.printLine("You must specify a file name on the command line"))
      _        <- ZIO.debug(fileName)
      contents <- readFile(fileName)
      _        <- Console.printLine(contents)
    } yield ()
}

object CatAcquireRelease extends ZIOAppDefault {

  import java.io.IOException
  import scala.io.Source

  def open(file: String): ZIO[Any, IOException, Source] =
    ZIO.attemptBlockingIO(scala.io.Source.fromFile(file))

  def close(source: Source): ZIO[Any, IOException, Unit] =
    ZIO.attemptBlockingIO(source.close())

  /** EXERCISE
    *
    * Using `ZIO#acquireReleaseWith`, implement a safe version of `readFile`
    * that cannot fail to close the file, no matter what happens during reading.
    */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO.acquireReleaseWith {
      ZIO.debug("ACQUIRE") *>
        open(file) // acquire
    } { source =>
      ZIO.debug("RELEASE") *>
        close(source).orDie // release
    } { source =>
      ZIO.debug("USE") *>
        ZIO.attemptBlockingIO(source.mkString) // use that resource
    }

  val run =
    for {
      args <- getArgs
      fileName <- ZIO
                    .fromOption(args.headOption)
                    .tapError(_ => Console.printLine("You must specify a file name on the command line"))
      contents <- readFile(fileName)
      _        <- Console.printLine(contents)
    } yield ()
}

final case class Skope(finalizers: Ref[List[UIO[Any]]]) {
  def addFinalizer(finalizer: UIO[Any]): UIO[Unit] =
    finalizers.update(finalizer :: _)

  val closeAll: UIO[Unit] =
    finalizers.get.flatMap { finalizers =>
      ZIO.collectAllDiscard(finalizers)
    }
}

object Skope {
  def make: UIO[Skope] =
    Ref.make(List.empty[UIO[Any]]).map(Skope(_))
}

object SkopeExample extends ZIOAppDefault {

  import java.io.IOException

  import scala.io.Source

  final class ZSource private (private val source: Source) {
    def execute[T](f: Source => T): ZIO[Any, IOException, T] =
      ZIO.attemptBlockingIO(f(source))
  }

  def acquireRelease[R, E, A](acquire: ZIO[R, E, A])(release: A => UIO[Any]): ZIO[R with Skope, E, A] =
    ZIO.uninterruptible {
      for {
        skope <- ZIO.service[Skope]
        a     <- acquire
        _     <- skope.addFinalizer(release(a))
      } yield a
    }

  def skoped[R: Tag, E, A](zio: ZIO[Skope with R, E, A]): ZIO[R, E, A] =
    for {
      skope <- Skope.make
      a     <- zio.provideSome[R](ZLayer.succeed(skope)).ensuring(skope.closeAll)
    } yield a

  object ZSource {

    /** EXERCISE
      *
      * Use the `ZIO.acquireRelease` constructor to make a scoped effect that
      * succeeds with a `ZSource`, whose finalizer will close the opened
      * resource.
      */
    def make(file: String): ZIO[Skope, IOException, ZSource] = {
      // An effect that acquires the resource:
      val open =
        ZIO.debug(s"OPENING SOURCE $file") *>
          ZIO.attemptBlockingIO(new ZSource(Source.fromFile(file)))

      // A function that, when given the resource, returns an effect that
      // releases the resource:
      val close: ZSource => ZIO[Any, Nothing, Unit] =
        source =>
          ZIO.debug(s"CLOSING SOURCE $file") *>
            source.execute(_.close()).orDie

      acquireRelease(open)(close)
    }
  }

  /** EXERCISE
    *
    * Using `ZSource.make`, as well as `ZIO.scoped` (to define the scope in
    * which resources are open), read the contents of the specified file into a
    * `String`.
    */
  def readFile(file: String): ZIO[Any, IOException, String] =
    skoped {
      for {
        source   <- ZSource.make(file)
        contents <- source.execute(_.mkString)
      } yield contents
    }

  /** EXERCISE
    *
    * Write an app that dumps the contents of the files specified by the
    * command-line arguments to standard out.
    */
  val run =
    for {
      args <- getArgs
      _ <-
        ZIO.foreachDiscard(args) { arg =>
          readFile(arg).debug("CONTENT")
        }
    } yield ()
}

object SourceScoped extends ZIOAppDefault {

  import java.io.IOException

  import scala.io.Source

  final class ZSource private (private val source: Source) {
    def execute[T](f: Source => T): ZIO[Any, IOException, T] =
      ZIO.attemptBlockingIO(f(source))
  }

  object ZSource {

    /** EXERCISE
      *
      * Use the `ZIO.acquireRelease` constructor to make a scoped effect that
      * succeeds with a `ZSource`, whose finalizer will close the opened
      * resource.
      */
    def make(file: String): ZIO[Scope, IOException, ZSource] = {
      // An effect that acquires the resource:
      val open =
        ZIO.debug(s"OPENING SOURCE $file") *>
          ZIO.attemptBlockingIO(new ZSource(Source.fromFile(file)))

      // A function that, when given the resource, returns an effect that
      // releases the resource:
      val close: ZSource => ZIO[Any, Nothing, Unit] =
        source =>
          ZIO.debug(s"CLOSING SOURCE $file") *>
            source.execute(_.close()).orDie

      ZIO.acquireRelease(open)(close)
    }
  }

  /** EXERCISE
    *
    * Using `ZSource.make`, as well as `ZIO.scoped` (to define the scope in
    * which resources are open), read the contents of the specified file into a
    * `String`.
    */
  def readFile(file: String): ZIO[Any, IOException, String] =
    ZIO.scoped {
      for {
        source   <- ZSource.make(file)
        contents <- source.execute(_.mkString)
      } yield contents
    }

  /** EXERCISE
    *
    * Write an app that dumps the contents of the files specified by the
    * command-line arguments to standard out.
    */
  val run =
    for {
      args <- getArgs
      _ <-
        ZIO.foreachDiscard(args) { arg =>
          readFile(arg).debug("CONTENT")
        }
    } yield ()
}

object CatIncremental extends ZIOAppDefault {

  import java.io.{FileInputStream, IOException, InputStream}

  final case class FileHandle private (private val is: InputStream) {
    def close: ZIO[Any, IOException, Unit] =
      ZIO.attemptBlockingIO(is.close())

    def read: ZIO[Any, IOException, Option[Chunk[Byte]]] =
      ZIO.attemptBlockingIO {
        val array = Array.ofDim[Byte](1024)
        val len   = is.read(array)
        if (len < 0) None
        else Some(Chunk.fromArray(array).take(len))
      }
  }

  /** EXERCISE
    *
    * Refactor `FileHandle` so that creating it returns a scoped effect, so that
    * it is impossible to forget to close an open handle.
    *
    * HINT: `ZIO.acquireRelease` is the easiest way to do this!
    */
  object FileHandle {
    final def open(file: String): ZIO[Any, IOException, FileHandle] =
      ZIO.attemptBlockingIO(new FileHandle(new FileInputStream(file)))
  }

  /** EXERCISE
    *
    * Implement an incremental version of `cat` that pulls a chunk of bytes at a
    * time, stopping when there are no more chunks left.
    */
  def cat(fh: FileHandle): ZIO[Any, IOException, Unit] =
    ???

  /** EXERCISE
    *
    * Implement an incremental version of the `cat` utility, using
    * `ZIO#acquireRelease` to ensure the file is closed in the event of error or
    * interruption.
    */
  val run =
    getArgs.map(_.toList).flatMap {
      case file :: Nil =>
        /** EXERCISE
          *
          * Open the specified file, safely create and use a file handle to
          * incrementally dump the contents of the file to standard output.
          */
        ???

      case _ => Console.printLine("Usage: cat <file>")
    }
}

object AddFinalizer extends ZIOAppDefault {

  /** EXERCISE
    *
    * Using `ZIO.addFinalizer`, which adds a finalizer to the currently open
    * scope, implement your own version of `ZIO.acquireRelease`. Note: The
    * version you implement need not be safe in the presence of interruption,
    * but it should be safe in the presence of errors.
    */
  def acquireRelease[R, E, A](acquire: ZIO[R, E, A])(release: A => ZIO[R, Nothing, Any]): ZIO[R with Scope, E, A] =
    ???

  val run =
    for {
      _ <- acquireRelease(Console.printLine("Acquired!"))(_ => Console.printLine("Released!").orDie)
      _ <- ZIO.fail("Uh oh!")
    } yield ()
}
