package json

import json.Json._
import json.PathElement.{Field, Index}
import monocle._
import monocle.functions.{Index => FIndex}
import monocle.classic.{Iso, Optional}

import scala.language.dynamics

case class JsonPath(path: List[PathElement], json: Optional[Json, Json]) extends Dynamic {

  val string: Optional[Json, String] =
    json.andThen(Json.jsonString)

  val int: Optional[Json, Int] =
    json.andThen(jsonInt)

  def selectDynamic(field: String): JsonPath = {
    val newPath = Field(field) :: path
    JsonPath(
      newPath,
      json.andThen(jsonObject).index(field)
    )
  }

  def index(key: Int): JsonPath = {
    val newPath = Index(key) :: path
    JsonPath(
      newPath,
      json.andThen(jsonArray).index(key)
    )
  }
}

object JsonPath {
  val root: JsonPath = JsonPath(Nil, Iso.id[Json])
}
