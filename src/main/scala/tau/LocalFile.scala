package tau

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import tau.WalkEvent.{PostVisitDirectory, PreVisitDirectory, VisitFile, VisitFileFailed}

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

object LocalFile {
  import scala.concurrent.ExecutionContext.Implicits.global

  def walkFileTree[S](path: Path, state: S)(fn: PartialFunction[(S, WalkEvent), VisitResult[S]]): Future[S] = {
    var currentState = state

    Future {
      Files.walkFileTree(path, new FileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult =
          dispatchEvent(PreVisitDirectory(dir, attrs))

        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult =
          dispatchEvent(VisitFile(file, attrs))

        override def visitFileFailed(file: Path, exc: IOException): FileVisitResult =
          dispatchEvent(VisitFileFailed(file, exc))

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult =
          dispatchEvent(PostVisitDirectory(dir, Option(exc)))

        private def dispatchEvent(walkEvent: WalkEvent): FileVisitResult =
          completeVisit(fn.applyOrElse(state -> walkEvent, (_: (S, WalkEvent)) => VisitResult.Continue(currentState)))

        private def completeVisit(result: VisitResult[S]): FileVisitResult = {
          currentState = result.state

          result match {
            case _: VisitResult.Continue[S] => FileVisitResult.CONTINUE
            case _: VisitResult.Terminate[S] => FileVisitResult.TERMINATE
            case _: VisitResult.SkipSubtree[S] => FileVisitResult.SKIP_SUBTREE
            case _: VisitResult.SkipSiblings[S] => FileVisitResult.SKIP_SIBLINGS
          }
        }
      })

      currentState
    }(BlockingIO.singleThreadExecutionContext)
  }

  def readFileContents(path: Path): Future[String] = {
    val channel = AsynchronousFileChannel.open(path, StandardOpenOption.READ)

    for {
      contents <- new FileReading(channel).readToEnd()
    } yield contents
  }

  private class FileReading(channel: AsynchronousFileChannel) {
    val buffer = ByteBuffer.allocate(1024)
    val contentBuilder = Array.newBuilder[Byte]

    var readPosition = 0

    def readToEnd(): Future[String] = {
      for {

        _ <- readRecursively()
      } yield new String(contentBuilder.result(), "UTF-8")
    }

    private def readRecursively(): Future[Unit] =
      for {
        len <- withCompletionHandler[Integer](completionHandler => channel.read[Any](buffer, readPosition, null, completionHandler))
        _ = contentBuilder ++= buffer.array().take(len)
        _ = readPosition += len
        _ = buffer.position(0)
        _ <- if (len >= 0) readRecursively() else Future.successful(())
      } yield ()

    private def withCompletionHandler[R](fn: CompletionHandler[R, Any] => Unit): Future[R] = {
      val promise = Promise[R]

      fn(new CompletionHandler[R, Any] {
        override def completed(result: R, attachment: Any): Unit =
          promise.complete(Success(result))

        override def failed(exc: Throwable, attachment: Any): Unit =
          promise.complete(Failure(exc))
      })

      promise.future
    }
  }
}

trait WalkEvent

object WalkEvent {
  case class VisitFile(file: Path, attrs: BasicFileAttributes) extends WalkEvent
  case class PreVisitDirectory(dir: Path, attrs: BasicFileAttributes) extends WalkEvent
  case class VisitFileFailed(file: Path, ex: IOException) extends WalkEvent
  case class PostVisitDirectory(dir: Path, ex: Option[IOException]) extends WalkEvent
}

trait VisitResult[S] {
  protected[tau] def state: S
}

object VisitResult {
  case class Continue[S](state: S) extends VisitResult[S]
  case class Terminate[S](state: S) extends VisitResult[S]
  case class SkipSubtree[S](state: S) extends VisitResult[S]
  case class SkipSiblings[S](state: S) extends VisitResult[S]
}