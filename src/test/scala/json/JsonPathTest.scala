package json

import json.Json.{JsonArray, JsonNumber, JsonObject, JsonString}
import json.PathElement.{Field, Index}

class JsonPathTest extends munit.FunSuite {

  val json: Json = JsonObject(Map(
    "http-server" -> JsonObject(Map(
      "hostname" -> JsonString("localhost"),
      "port"     -> JsonNumber(8080),
      "tokens"   -> JsonArray(List(
        JsonString("abc"), JsonString("xxx"), JsonString("123")
      ))
    )),
    "db" -> JsonObject(Map(
      "connection" -> JsonNumber(4),
      "url"        -> JsonString("jdbc:postgresql://localhost:5432/db")
    ))
  ))

  test("success") {
    assertEquals(
      JsonPath.root.`http-server`.hostname.string.getOrError(json),
      Right("localhost")
    )

    assertEquals(
      JsonPath.root.`http-server`.tokens.index(1).string.getOrError(json),
      Right("xxx")
    )

    assertEquals(
      JsonPath.root.db.connection.int.getOrError(json),
      Right(4)
    )
  }

  test("fail with missing field") {
    assertEquals(
      JsonPath.root.foo.bar.int.getOrError(json),
      Left(JsonPathError(List(Field("foo")), "Key is missing"))
    )

    assertEquals(
      JsonPath.root.db.foo.bar.int.getOrError(json),
      Left(JsonPathError(List(Field("foo"), Field("db")), "Key is missing"))
    )
  }

  // test("fail with missing index") {
  //   assertEquals(
  //     JsonPath.root.`http-server`.tokens.index(5).string.getOrError(json),
  //     Left(JsonPathError(List(Index(5), Field("tokens"), Field("http-server")), "Index is missing"))
  //   )
  // }

  // test("fail with wrong type") {
  //   assertEquals(
  //     JsonPath.root.db.connection.string.getOrError(json),
  //     Left(JsonPathError(List(Field("connection"), Field("db")), "Expected JsonString but got JsonNumber(4)"))
  //   )
  // }

}
