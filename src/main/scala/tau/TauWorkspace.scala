package tau

import java.nio.file._

import io.circe.generic.auto._
import io.circe.yaml.parser
import tau.WalkEvent.VisitFile

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

class TauWorkspace(path: String, projects: Seq[Project])(implicit executionContext: ExecutionContext) {

}

class TauWorkspaces(implicit executionContext: ExecutionContext) {

  def createAWorkspace(path: String): TauWorkspace =
    Await.result(createAWorkspaceAsync(path), Duration.Inf)

  private def createAWorkspaceAsync(path: String) = {
    for {
      projectFiles <- findProjectFiles(path)
      projects = projectFiles.map(projectFile => new TauProject(projectFile))
      targets <- Future.sequence(projects.map(_.targets))
      _ = println(s"Targets loaded: ${targets.flatten.size}")
    } yield new TauWorkspace(path, projects)
  }

  private def findProjectFiles(path: String): Future[Seq[Path]] =
    for {
      paths <- LocalFile.walkFileTree(Paths.get(path), Seq.newBuilder[Path]) {
        case (state, VisitFile(path, _)) if path.endsWith("tau.project") =>
          VisitResult.Continue(state += path)
      }
    } yield paths.result()
}

trait Project {
  def targets: Future[Seq[Target]]
}

class TauProject(path: Path)(implicit executionContext: ExecutionContext) extends Project {
  private lazy val projectDescriptor: Future[TauProjectDescriptor] =
    for {
      content <- LocalFile.readFileContents(path)
      descriptor = parser.parse(content).right.get.as[TauProjectDescriptor].right.get
    } yield descriptor

  override def targets: Future[Seq[Target]] =
    for {
      descriptor <- projectDescriptor
      _ = println(descriptor)
    } yield Nil
}

trait Target {
  def associatedPath: Option[Path]
}

case class TauProjectDescriptor(
    packages: Seq[String],
    coords: Map[String, CoordInfo]
)

case class CoordInfo(version: String)