package letout

sealed trait DependencyReference

object DependencyReference {
  case class OtherTargetDependency(target: String) extends DependencyReference

  case class BookmarkedDependency(
      bookmark: String,
      path: Option[String]
  ) extends DependencyReference

  case class LocalDependency(path: String) extends DependencyReference

  def parse(dependencyRef: String): DependencyReference =
    dependencyRef match {
      case ref if ref.startsWith("$") =>
        val endOfBookmarkPos = Some(ref.indexOf("#!")).filterNot(_ == -1).getOrElse(ref.length)

        DependencyReference.BookmarkedDependency(
          bookmark = dependencyRef.substring(1, endOfBookmarkPos),
          path = Some(dependencyRef.substring(endOfBookmarkPos)).filterNot(_ == "")
        )
      case ref if ref.startsWith("/") =>
        DependencyReference.OtherTargetDependency(ref)
      case ref =>
        DependencyReference.LocalDependency(ref)
    }
}
