package letout

import scala.util.{Failure, Success, Try}

object Results {
  def seqOfTryToTryOfSeq[T](tries: Iterable[Try[T]]): Try[Seq[T]] = {
    tries.find(_.isFailure) match {
      case Some(Failure(f)) => Failure(f)
      case _ => Success(tries.map(_.get).toSeq)
    }
  }
}
