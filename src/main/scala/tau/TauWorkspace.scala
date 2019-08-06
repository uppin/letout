package tau

import java.nio.file._

import io.circe.JsonObject
import io.circe.generic.auto._
import monix.execution.Scheduler
import monix.reactive.Consumer
import tau.WalkEvent.{PreVisitDirectory, VisitFile}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait TauWorkspace {
  def build: Future[Unit]
}

class TauWorkspaces(
    targetScheduler: TargetScheduler,
    targetBuilder: TargetBuilder)(implicit executionContext: ExecutionContext) {

  def createAWorkspace(path: String): TauWorkspace =
    Await.result(createAWorkspaceAsync(path), Duration.Inf)

  private def createAWorkspaceAsync(path: String): Future[TauWorkspace] = {
    for {
      projectFiles <- findProjectFiles(path)
      projects = projectFiles.map(projectFile => new TauProject(projectFile))
      targets <- Future.sequence(projects.map(_.targets))
      _ = println(s"${targets.flatten.size} targets loaded in ${projects.size} projects")
    } yield new IntTauWorkspace(path, projects)
  }

  private def findProjectFiles(path: String): Future[Seq[Path]] =
    for {
      paths <- LocalFiles.walkFileTree(Paths.get(path), Seq.newBuilder[Path]) {
        case (state, VisitFile(path, _)) if path.endsWith("tau.project") =>
          VisitResult.Continue(state += path)
      }
    } yield paths.result()

  private class IntTauWorkspace(path: String, projects: Seq[Project])(implicit executionContext: ExecutionContext) extends TauWorkspace {

    implicit val scheduler = Scheduler(executionContext)

    def build: Future[Unit] = {
      val targetQueue = targetScheduler.processBuildsInOrder(projects)

      targetQueue.consumeWith(Consumer.foreach(target => println("== BUILD", target.coord))).runAsync
    }
  }
}

trait Project {
  def projectDescriptor: Future[TauProjectDescriptor]
  def targets: Future[Seq[Target]]
}

class TauProject(path: Path)(implicit executionContext: ExecutionContext) extends Project {

  private val directory = path.getParent

  lazy val projectDescriptor: Future[TauProjectDescriptor] =
    for {
      descriptor <- Yaml.parseFileContents[TauProjectCodecDescriptor](path)
    } yield descriptor.toDescriptor(path)

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
  def deps: Set[TargetDependencyReference]
}

case class TauTarget(
    path: Path,
    descriptor: TauTargetDescriptor
) extends Target {

  def associatedPath: Option[Path] = Some(path)

  override def deps: Set[TargetDependencyReference] = (descriptor.deps.getOrElse(Nil).map(TargetDependencyReference(_, DependencyScope.Compile)) ++
    descriptor.runtimeDeps.getOrElse(Nil).map(TargetDependencyReference(_, DependencyScope.Runtime))).toSet
}

case class TargetCoord(path: String, name: String)

object TauTargetsFile {

  def read(path: Path)(implicit executionContext: ExecutionContext): Future[Seq[TauTarget]] =
    for {
      targetDescriptors <- Yaml.parseFileContents[Seq[TauTargetDescriptor]](path)
    } yield targetDescriptors.map(desc => TauTarget(path, desc))
}

case class TauProjectDescriptor(
    path: Path,
    sourceRoots: Seq[String],
    mounts: Map[String, String],
    bookmarks: Map[String, String]
)

case class TauProjectCodecDescriptor(
    sourceRoots: Option[Seq[String]],
    mounts: Option[Map[String, String]],
    bookmarks: Option[Map[String, String]]
) {
  def toDescriptor(path: Path) =
    TauProjectDescriptor(
      path = path,
      sourceRoots = sourceRoots.getOrElse(Nil),
      mounts = mounts.getOrElse(Map.empty),
      bookmarks = bookmarks.getOrElse(Map.empty)
    )
}

case class TauTargetDescriptor(
    name: String,
    `type`: String,
    props: Option[JsonObject],
    sources: Option[Seq[String]],
    deps: Option[Seq[String]],
    runtimeDeps: Option[Seq[String]]
)

case class TargetDependencyReference(
    reference: DependencyReference,
    scope: DependencyScope
)

case class TargetDependency(
    reference: Dependency,
    scope: DependencyScope
)

object TargetDependencyReference {
  def apply(dependencyRef: String, scope: DependencyScope): TargetDependencyReference =
    TargetDependencyReference(DependencyReference.parse(dependencyRef), scope)
}

sealed trait DependencyScope

object DependencyScope {
  case object Compile extends DependencyScope
  case object Runtime extends DependencyScope
}
