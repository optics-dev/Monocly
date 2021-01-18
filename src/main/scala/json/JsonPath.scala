package json

import json.Json._
import json.PathElement.{Field, Index}
import optics.poly.{EOptional, Iso}
import optics.poly.functions.{Index => FIndex}

import scala.language.dynamics

case class JsonPath(path: List[PathElement], json: EOptional[JsonPathError, Json, Json]) extends Dynamic {

  val string: EOptional[JsonPathError, Json, String] =
    json >>> jsonString.mapError(JsonPathError(path, _))

  val int: EOptional[JsonPathError, Json, Int] =
    json >>> jsonInt.mapError(JsonPathError(path, _))

  def selectDynamic(field: String): JsonPath = {
    val newPath = Field(field) :: path
    JsonPath(
      newPath,
      json >>> jsonObject.mapError(JsonPathError(path, _)) >>> FIndex.withError(field, JsonPathError(newPath, "Key is missing"))
    )
  }

  def index(key: Int): JsonPath = {
    val newPath = Index(key) :: path
    JsonPath(
      newPath,
      (json >>> jsonArray.mapError(JsonPathError(path, _))).indexError(key, JsonPathError(newPath, "Index is missing"))
    )
  }
}

object JsonPath {
  val root: JsonPath = JsonPath(Nil, Iso.id)
}
