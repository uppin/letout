package letout

import java.nio.file._

import io.circe.JsonObject
import io.circe.generic.auto._
import monix.execution.Scheduler
import monix.reactive.Consumer
import letout.WalkEvent.{PreVisitDirectory, VisitFile}

import scala.concurrent.{ExecutionContext, Future}

trait LetoutWorkspace {
  def build(implicit callScope: CallScope): Future[Unit]
}

class LetoutWorkspaces(
    targetScheduler: TargetScheduler,
    targetBuilder: TargetBuilder)(implicit executionContext: ExecutionContext) {

  def createAWorkspace(path: String)(implicit callScope: CallScope): Future[LetoutWorkspace] = {
    for {
      projectFiles <- findProjectFiles(path)
      projects = projectFiles.map(projectFile => new LetoutProject(projectFile))
      targets <- Future.sequence(projects.map(_.targets))
      _ = callScope.println(s"${targets.flatten.size} targets loaded in ${projects.size} projects")
    } yield IntLetoutWorkspace(path, projects)
  }

  private def findProjectFiles(path: String): Future[Seq[Path]] =
    for {
      paths <- LocalFiles.walkFileTree(Paths.get(path), Seq.newBuilder[Path]) {
        case (state, VisitFile(path, _)) if path.endsWith("tau.project") =>
          VisitResult.Continue(state += path)
      }
    } yield paths.result()

  private case class IntLetoutWorkspace(path: String, projects: Seq[Project])(implicit executionContext: ExecutionContext) extends LetoutWorkspace {

    implicit val scheduler = Scheduler(executionContext)

    def build(implicit callScope: CallScope): Future[Unit] = {
      val targetQueue = targetScheduler.processBuildsInOrder(projects)

      targetQueue.consumeWith(Consumer.foreach(target => callScope.println("== BUILD", target.coord))).runAsync
    }
  }
}

trait Project {
  def projectDescriptor: Future[LetoutProjectDescriptor]
  def targets: Future[Seq[Target]]
}

case class LetoutProject(path: Path)(implicit executionContext: ExecutionContext) extends Project {

  private val directory = path.getParent

  lazy val projectDescriptor: Future[LetoutProjectDescriptor] =
    for {
      descriptor <- Yaml.parseFileContents[LetoutProjectCodecDescriptor](path)
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
          VisitResult.Continue(state += LetoutTargetsFile.read(path))
      }.map(state => state.result())
      targets <- Future.sequence(targetFutures)
    } yield targets.flatten
}

trait Target {
  def associatedPath: Option[Path]
  def deps: Set[TargetDependencyReference]
}

case class LetoutTarget(
    path: Path,
    descriptor: LetoutTargetDescriptor
) extends Target {

  def associatedPath: Option[Path] = Some(path)

  override def deps: Set[TargetDependencyReference] = (descriptor.deps.getOrElse(Nil).map(TargetDependencyReference(_, DependencyScope.Compile)) ++
    descriptor.runtimeDeps.getOrElse(Nil).map(TargetDependencyReference(_, DependencyScope.Runtime))).toSet
}

case class TargetCoord(path: String, name: String)

object LetoutTargetsFile {

  def read(path: Path)(implicit executionContext: ExecutionContext): Future[Seq[LetoutTarget]] =
    for {
      targetDescriptors <- Yaml.parseFileContents[Seq[LetoutTargetDescriptor]](path)
    } yield targetDescriptors.map(desc => LetoutTarget(path, desc))
}

case class LetoutProjectDescriptor(
    path: Path,
    sourceRoots: Seq[String],
    mounts: Map[String, String],
    bookmarks: Map[String, String]
)

case class LetoutProjectCodecDescriptor(
    sourceRoots: Option[Seq[String]],
    mounts: Option[Map[String, String]],
    bookmarks: Option[Map[String, String]]
) {
  def toDescriptor(path: Path) =
    LetoutProjectDescriptor(
      path = path,
      sourceRoots = sourceRoots.getOrElse(Nil),
      mounts = mounts.getOrElse(Map.empty),
      bookmarks = bookmarks.getOrElse(Map.empty)
    )
}

case class LetoutTargetDescriptor(
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
