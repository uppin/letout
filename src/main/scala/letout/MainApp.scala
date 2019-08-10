package letout

import java.io.{Closeable, InputStream, PrintStream}
import java.util.concurrent.Executors

import monix.execution.Scheduler

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

object MainApp extends App {

  sys.addShutdownHook {
    BlockingIO.shutdown()
  }

  val context = new AppContext

  try Await.result(context.run(args)(CallScope(StdIO(System.in, System.out, System.err))), Duration.Inf)
  finally context.close
}

object MainAppNG {

  import com.facebook.nailgun.{NGContext, NGServer}

  val appContext = new AppContext

  def nailMain(context: NGContext): Unit = {
    Await.result(appContext.run(context.getArgs)(CallScope(StdIO(context.in, context.out, context.err))), Duration.Inf)
  }

  def nailShutdown(server: NGServer): Unit = {
    appContext.close()
    BlockingIO.shutdown()
  }
}

class AppContext extends Closeable {

  val executor = Executors.newFixedThreadPool(4)

  implicit val ec = Scheduler(executor)

  val targetScheduler = new TargetScheduler
  val targetBuilder = new TargetBuilder

  val workspaces = new LetoutWorkspaces(targetScheduler, targetBuilder)

  def run(args: Array[String])(implicit callScope: CallScope): Future[Unit] =
    for {
      workspace <- workspaces.createAWorkspace("/Users/mantas/Uppin")
      _ <- workspace.build
    } yield ()

  override def close(): Unit = {
    executor.shutdown()
    ec.shutdown()
  }
}

case class StdIO(in: InputStream, out: PrintStream, err: PrintStream)