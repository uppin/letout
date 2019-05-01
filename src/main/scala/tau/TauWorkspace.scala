package tau

import java.nio.file._

import io.circe.{Decoder, Json, JsonObject}
import io.circe.generic.auto._
import io.circe.yaml.parser
import tau.WalkEvent.{PreVisitDirectory, VisitFile}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class TauWorkspace(path: String, projects: Seq[Project])(implicit executionContext: ExecutionContext) {

  def build: Future[Unit] = {
    ???
  }
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
      paths <- LocalFiles.walkFileTree(Paths.get(path), Seq.newBuilder[Path]) {
        case (state, VisitFile(path, _)) if path.endsWith("tau.project") =>
          VisitResult.Continue(state += path)
      }
    } yield paths.result()
}

trait Project {
  def targets: Future[Seq[Target]]
}

class TauProject(path: Path)(implicit executionContext: ExecutionContext) extends Project {

  private val directory = path.getParent

  private lazy val projectDescriptor: Future[TauProjectDescriptor] =
    for {
      content <- LocalFiles.readFileContents(path)
      _ = println()
      descriptor <- Future.fromTry(Yaml.parse[TauProjectDescriptor](content))
    } yield descriptor

  override def targets: Future[Seq[Target]] =
    for {
      descriptor <- projectDescriptor
      sourceRoots <- LocalFiles.walkFileTree(directory, Seq.newBuilder[Path]) {
        case (state, PreVisitDirectory(path, _)) if descriptor.sourceRoots.find(m => path.toString.endsWith(m)).nonEmpty =>
          VisitResult.Continue(state += path)
      }.map(state => state.result())
      targets <- Future.sequence(sourceRoots.map(root => collectTargets(root)))
    } yield targets.flatten

  private def collectTargets(sourceRoot: Path): Future[Seq[Target]] =
    for {
      targetFutures <- LocalFiles.walkFileTree(sourceRoot, Seq.newBuilder[Future[Seq[Target]]]) {
        case (state, VisitFile(path, _)) if path.endsWith("tau.targets") =>
          VisitResult.Continue(state += TauTargetsFile.read(path))
      }.map(state => state.result())
      targets <- Future.sequence(targetFutures)
    } yield targets.flatten
}

trait Target {
  def associatedPath: Option[Path]
}

case class TauTarget(
    path: Path,
    descriptor: TauTargetDescriptor
) extends Target {

  def associatedPath: Option[Path] = Some(path)
}

object TauTargetsFile {

  def read(path: Path)(implicit executionContext: ExecutionContext): Future[Seq[TauTarget]] =
    for {
      targetsSource <- LocalFiles.readFileContents(path)
      targetDescriptors <- Future.fromTry(Yaml.parse[Seq[TauTargetDescriptor]](targetsSource))
    } yield targetDescriptors.map(desc => TauTarget(path, desc))
}

case class TauProjectDescriptor(
    sourceRoots: Seq[String],
    coords: Map[String, String]
)

case class TauTargetDescriptor(
    name: String,
    `type`: String,
    props: Option[JsonObject],
    sources: Option[Seq[String]],
    deps: Option[Seq[String]],
    runtimeDeps: Option[Seq[String]]
)

object Yaml {

  def parse[R](content: String)(implicit decoder: Decoder[R]): Try[R] = {
    parser.parse(content) match {
      case Right(json) => read[R](json)
      case Left(error) => Failure(error)
    }
  }

  private def read[R](json: Json)(implicit decoder: Decoder[R]): Try[R] = {
    json.as[R] match {
      case Right(value) => Success(value)
      case Left(error) => Failure(error)
    }
  }
}
