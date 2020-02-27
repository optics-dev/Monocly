package optics.poly.tc

import optics.poly.Optional

import scala.util.Try

trait Index1[From, -Key, To] {
  def index(key: Key): Optional[From, To]

}

object Index1 {
  def index[From, Key, To](key: Key)(implicit ev: Index1[From, Key, To]): Optional[From, To] = ev.index(key)

  implicit def mapIndex[K, V]: Index1[Map[K, V], K, V] = new Index1[Map[K, V], K, V]  {
    def index(key: K): Optional[Map[K, V], V] =
      Optional.indexMap(key)
  }

  implicit def listIndex[A]: Index1[List[A], Int, A] = new Index1[List[A], Int, A] {
    def index(key: Int): Optional[List[A], A] =
      if (key < 0)
        Optional.void
      else
        Optional[List[A], A](_.drop(key).headOption, a => xs => Try(xs.updated(key, a)).getOrElse(xs))
  }
}


