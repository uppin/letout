package tau

import monix.reactive.Observer

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object Observers {

  def completeFrom[T](observer: Observer[_], result: Future[T])(implicit executionContext: ExecutionContext): Future[T] = {
    result.onComplete {
      case Success(_) => observer.onComplete()
      case Failure(e) => observer.onError(e)
    }

    result
  }
}
