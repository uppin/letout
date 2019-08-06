package tau

import java.nio.file.Path

import io.circe.{Decoder, Json}
import io.circe.yaml.parser

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

object Yaml {

  case class YamlFailedToParse(path: Path, cause: Throwable) extends RuntimeException("" +
    s"Failed to parse Yaml file on path ${path}", cause)

  import scala.concurrent.ExecutionContext.Implicits.global

  def parseFileContents[T](path: Path)(implicit decoder: Decoder[T]): Future[T] =
    for {
      content <- LocalFiles.readFileContents(path)
      result <- Future.fromTry(parse[T](content).recover {
        case e: Throwable => throw YamlFailedToParse(path, e)
      })
    } yield result


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
