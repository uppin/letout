package tau

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

object MainApp extends App {

  val executor = Executors.newFixedThreadPool(4)

  implicit val ec = ExecutionContext.fromExecutorService(executor)

  val workspaces = new TauWorkspaces()

  try workspaces.createAWorkspace("/Users/mantas/Uppin")
  catch {
    case e: Throwable => e.printStackTrace(System.err)
  }
  finally {
    executor.shutdown()
    BlockingIO.shutdown()
  }
}
