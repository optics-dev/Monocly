package json

import optics.poly.EPrism

sealed trait Json

object Json {
  case class JsonNumber(value: Int) extends Json
  case class JsonString(value: String) extends Json
  case class JsonBoolean(value: Boolean) extends Json
  case class JsonObject(value: Map[String, Json]) extends Json
  case class JsonArray(value: List[Json]) extends Json

  val jsonString: EPrism[String, Json, String] =
    EPrism[String, Json, String](
      {
        case JsonString(str) => Right(str)
        case other           => Left(s"Expected JsonString but got $other")
      }, JsonString.apply
    )

  val jsonInt: EPrism[String, Json, Int] =
    EPrism[String, Json, Int](
      {
        case JsonNumber(num) => Right(num)
        case other           => Left(s"Expected JsonNumber but got $other")
      }, JsonNumber.apply
    )

  val jsonObject: EPrism[String, Json, Map[String, Json]] =
    EPrism[String, Json, Map[String, Json]](
      {
        case JsonObject(map) => Right(map)
        case other           => Left(s"Expected JsonObject but got $other")
      }, JsonObject.apply
    )

  val jsonArray: EPrism[String, Json, List[Json]] =
    EPrism[String, Json, List[Json]](
      {
        case JsonArray(list) => Right(list)
        case other           => Left(s"Expected JsonArray but got $other")
      }, JsonArray.apply
    )
}
