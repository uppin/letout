package letout

import monix.execution.Scheduler
import monix.reactive.{MulticastStrategy, Observable, Observer}
import letout.Dependency.{MavenJar, OtherTargetDependency}
import letout.ProjectWithDescriptor.UnresolvedDependency
import letout.Results.seqOfTryToTryOfSeq
import letout.TargetScheduler.UnresolvedDependencyInTarget

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class TargetScheduler(implicit scheduler: Scheduler) {

  def processBuildsInOrder(projects: Seq[Project]): Observable[ScheduledTarget] = {
    val (observer, observable) = Observable.multicast[ScheduledTarget](MulticastStrategy.Replay(Nil))

    processScheduling(projects, observer).recover {
      case e: Throwable =>
        observer.onError(e)
    }

    observable
  }

  private def processScheduling(projects: Seq[Project], observer: Observer[ScheduledTarget]) = {
    for {
      allProjectCoords <- Future.sequence(projects.map(project => project.projectDescriptor.map(coords => ProjectWithDescriptor(project, coords))))
      orderedProjects = orderProjects(allProjectCoords)
      _ <- SchedulingState(orderedProjects.toList, observer).processTargets()
    } yield ()
  }

  private def orderProjects(allProjectCoords: Seq[ProjectWithDescriptor]): Seq[ProjectWithDescriptor] =
    allProjectCoords.sortBy(_.descriptor.bookmarks.size)
}

object TargetScheduler {
  case class UnresolvedDependencyInTarget(target: Target, ref: String)
    extends RuntimeException(s"Unresolved dependency $ref in $target")
}

private case class ProjectTargetsContext(
    project: ProjectWithDescriptor,
    remainingTargets: Seq[Target])

private class SchedulingState private(
    remainingTargets: TrieMap[TargetCoord, ScheduledTarget],
    projects: Seq[ProjectWithDescriptor],
    output: Observer[ScheduledTarget])(implicit executionContext: ExecutionContext) {

  def processTargets(): Future[Unit] = {
    Observers.completeFrom(output,
      for {
        targets <- Future.sequence(projects.map(loadProjectTargets(_)))
        _ = remainingTargets ++= targets.flatten.map(target => target.coord -> target)
        _ = processASingleRun()
  //      _ = remainingTargets.values.foreach(output.onNext(_))
      } yield ())
  }

  private def processASingleRun() = {
    remainingTargets.keys.toSeq.sortBy(coord => remainingTargets(coord).deps.size).foreach { coord =>
      val toRelease = remainingTargets.get(coord).toSeq.flatMap(resolveReleasibleFor(_, coord))

      toRelease.foreach { coord =>
        remainingTargets.remove(coord).foreach(target => output.onNext(target))
      }
    }
  }

  private def resolveReleasibleFor(target: ScheduledTarget, coord: TargetCoord): Seq[TargetCoord] = {
    val targetDeps = target.deps.collect {
      case TargetDependency(OtherTargetDependency(target), _) => TargetCoord(target, "")
    }

    val (_, remaining) = targetDeps.partition(dep => remainingTargets.contains(dep))

    if (remaining.isEmpty)
      Seq(coord)
    else
      (for {
        directCoord <- remaining.toSeq
        target <- remainingTargets.get(directCoord).toSeq
        depCoord <- resolveReleasibleFor(target, directCoord)
      } yield depCoord) :+ coord
  }

  private def loadProjectTargets(projectWithDesc: ProjectWithDescriptor): Future[Seq[ScheduledTarget]] =
    for {
      targets <- projectWithDesc.project.targets
      scheduledTargets <- Future.fromTry(seqOfTryToTryOfSeq(targets.map(target => scheduledTargetFrom(projectWithDesc, target))))
    } yield scheduledTargets

  private def scheduledTargetFrom(projectWithDesc: ProjectWithDescriptor, target: Target): Try[ScheduledTarget] =
    for {
      deps <- seqOfTryToTryOfSeq(target.deps.map(targetDep =>
        projectWithDesc.resolveDependency(targetDep.reference).map(TargetDependency(_, targetDep.scope)).recoverWith {
          case UnresolvedDependency(ref) => Failure(UnresolvedDependencyInTarget(target, ref))
        }))
    } yield ScheduledTarget(targetCoordFor(target, projectWithDesc.descriptor), target, deps.toSet)

  private def targetCoordFor(target: Target, descriptor: LetoutProjectDescriptor): TargetCoord =
    TargetCoord(descriptor.path.relativize(target.associatedPath.get).toString, "")
}

private object SchedulingState {

    def apply(projects: Seq[ProjectWithDescriptor], output: Observer[ScheduledTarget])(implicit executionContext: ExecutionContext): SchedulingState =
      new SchedulingState(TrieMap.empty[TargetCoord, ScheduledTarget], projects, output)
}

private case class ProjectWithDescriptor(project: Project, descriptor: LetoutProjectDescriptor) {

  def resolveDependency(dep: DependencyReference): Try[Dependency] =
    dep match {
      case DependencyReference.BookmarkedDependency(bookmark, None) =>
        for {
          bookmarkDesc <- descriptor.bookmarks.get(bookmark).map(Success(_)).getOrElse(Failure(UnresolvedDependency(bookmark)))
          Array(groupId, artifactId) = bookmark.split(':')
        } yield MavenJar(groupId, artifactId, bookmarkDesc)
      case DependencyReference.OtherTargetDependency(target) =>
        Success(OtherTargetDependency(target))
    }
}

private object ProjectWithDescriptor {
  case class UnresolvedDependency(ref: String) extends RuntimeException(s"Invalid dependency reference $ref")
}

case class ScheduledTarget(
    coord: TargetCoord,
    target: Target,
    deps: Set[TargetDependency])