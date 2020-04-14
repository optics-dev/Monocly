package json

case class JsonPathError(path: List[PathElement], message: String)
