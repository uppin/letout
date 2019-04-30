package tau

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext

object MainApp extends App {

  val executor = Executors.newFixedThreadPool(4)

  implicit val ec = ExecutionContext.fromExecutorService(executor)

  val workspaces = new TauWorkspaces()

  workspaces.createAWorkspace("/Users/mantas/Uppin")

  executor.shutdown()
  BlockingIO.shutdown()
}
