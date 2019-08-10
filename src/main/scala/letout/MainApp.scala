package letout

import java.util.concurrent.Executors

import monix.execution.Scheduler

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MainApp extends App {

  val executor = Executors.newFixedThreadPool(4)

  implicit val ec = Scheduler(executor)

  sys.addShutdownHook {
    BlockingIO.shutdown()
  }

  val targetScheduler = new TargetScheduler
  val targetBuilder = new TargetBuilder

  val workspaces = new LetoutWorkspaces(targetScheduler, targetBuilder)

  try {
    val workspace = workspaces.createAWorkspace("/Users/mantas/Uppin")

    Await.result(workspace.build, Duration.Inf)
  }
  catch {
    case e: Throwable => e.printStackTrace(System.err)
  } finally {
    executor.shutdown()
    ec.shutdown()
  }
}
