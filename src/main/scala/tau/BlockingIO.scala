package tau

import java.util.concurrent.{Executors, Future => JFuture}
import java.util.{Timer, TimerTask}

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Try

object BlockingIO {
  private val timer = new Timer()
  private val javaFutures = TrieMap.empty[JFuture[Any], Promise[Any]]

  val singleThreadExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

  timer.scheduleAtFixedRate(new TimerTask {
    override def run(): Unit = {
      val completedFutures = javaFutures.collect {
        case (future, _) if future.isDone || future.isCancelled => future
      }

      completedFutures.foreach {
        case future =>
          for {
            promise <- javaFutures.remove(future)
          } yield promise.complete(Try(future.get()))
      }
    }
  }, 1, 1)

  implicit class FutureConverter[T](private val future: JFuture[T]) {
    def asScala: Future[T] = submitAFuture(future)
  }

  def shutdown(): Unit = {
    timer.cancel()
    singleThreadExecutionContext.shutdown()
  }

  private def submitAFuture[T](future: JFuture[T]): Future[T] = {
    val promise = Promise[T]

    javaFutures.put(future.asInstanceOf[JFuture[Any]], promise.asInstanceOf[Promise[Any]])

    promise.future
  }
}
