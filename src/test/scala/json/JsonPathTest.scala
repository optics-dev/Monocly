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
      JsonPath.root.`http-server`.hostname.string.getOption(json),
      Some("localhost")
    )

    assertEquals(
      JsonPath.root.`http-server`.tokens.index(1).string.getOption(json),
      Some("xxx")
    )

    assertEquals(
      JsonPath.root.db.connection.int.getOption(json),
      Some(4)
    )
  }

  test("fail with missing field") {
    assertEquals(
      JsonPath.root.foo.bar.int.getOption(json),
      None
    )

    assertEquals(
      JsonPath.root.db.foo.bar.int.getOption(json),
      None
    )
  }

  test("fail with missing index") {
    assertEquals(
      JsonPath.root.`http-server`.tokens.index(5).string.getOption(json),
      None
    )
  }

  test("fail with wrong type") {
    assertEquals(
      JsonPath.root.db.connection.string.getOption(json),
      None
    )
  }

}
