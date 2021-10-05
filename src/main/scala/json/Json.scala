package json

import monocle.classic.*

sealed trait Json

object Json {
  case class JsonNumber(value: Int) extends Json
  case class JsonString(value: String) extends Json
  case class JsonBoolean(value: Boolean) extends Json
  case class JsonObject(value: Map[String, Json]) extends Json
  case class JsonArray(value: List[Json]) extends Json

  val jsonString: Prism[Json, String] =
    Prism.partial[Json, String]{case JsonString(str) => str}(JsonString.apply)

  val jsonInt: Prism[Json, Int] =
    Prism.partial[Json, Int]{ case JsonNumber(x) => x}(JsonNumber.apply)

  val jsonObject: Prism[Json, Map[String, Json]] =
    Prism.partial[Json, Map[String, Json]]{case JsonObject(x) => x}(JsonObject.apply)

  val jsonArray: Prism[Json, List[Json]] =
    Prism.partial[Json, List[Json]]{case JsonArray(x) => x}(JsonArray.apply)
}
