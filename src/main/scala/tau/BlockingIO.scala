package tau

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

object BlockingIO {
//  private val timer = new Timer()
//  private val javaFutures = TrieMap.empty[JFuture[Any], Promise[Any]]

  val blockingExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

//  timer.scheduleAtFixedRate(new TimerTask {
//    override def run(): Unit = {
//      val completedFutures = javaFutures.collect {
//        case (future, _) if future.isDone || future.isCancelled => future
//      }
//
//      completedFutures.foreach {
//        case future =>
//          for {
//            promise <- javaFutures.remove(future)
//          } yield promise.complete(Try(future.get()))
//      }
//    }
//  }, 1, 1)
//
//  implicit class FutureConverter[T](private val future: JFuture[T]) {
//    def asScala: Future[T] = submitAFuture(future)
//  }

  def shutdown(): Unit = {
    blockingExecutionContext.shutdown()
  }
//
//  private def submitAFuture[T](future: JFuture[T]): Future[T] = {
//    val promise = Promise[T]
//
//    javaFutures.put(future.asInstanceOf[JFuture[Any]], promise.asInstanceOf[Promise[Any]])
//
//    promise.future
//  }
}
