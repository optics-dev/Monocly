package json

sealed trait PathElement

object PathElement {
  case class Field(value: String) extends PathElement
  case class Index(value: Int) extends PathElement
}
