package tau

case class Bookmarks private(bookmarks: Map[String, Bookmark])

object Bookmarks {
  def readFrom(coords: Map[String, String]): Bookmarks =
    Bookmarks(coords.map {
      case (name, value) => name -> Bookmark(value)
    })
}

case class Bookmark(value: String)
