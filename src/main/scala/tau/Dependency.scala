package tau

sealed trait Dependency

object Dependency {
  case class OtherTargetDependency(path: String) extends Dependency
  case class MavenJar(groupId: String, artifactId: String, version: String) extends Dependency
  case class TargetOnAnotherGitRepo(gitUrl: String, target: String) extends Dependency
}
