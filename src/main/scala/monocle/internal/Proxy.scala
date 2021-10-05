package monocle.internal

case class Proxy[+A]()

object Proxy {
  val nothing: Proxy[Nothing] = Proxy()
}