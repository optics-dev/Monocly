package optics.internal

case class Proxy[+A]()

object Proxy {
  val nothing: Proxy[Nothing] = Proxy()
}