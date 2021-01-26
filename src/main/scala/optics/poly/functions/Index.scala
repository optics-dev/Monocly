package optics.poly.functions
import optics.poly.Optional

import scala.collection.immutable.Map
import scala.util.Try

trait Index[From, -Key] {
  type To

  def index(i: Key): Optional[From, To]

}

object Index {

  def apply[Key, From, T](key: Key)(using idx: Index[From, Key] { type To = T}): Optional[From, T] =
    idx.index(key)

  def map[K, V](key: K): Optional[Map[K, V], V] =
    apply(key)

  def list[A](i: Int): Optional[List[A], A] =
    apply[Int, List[A], A](i)

  given [A]: Index[List[A], Int] with {
    type To = A

    def index(i: Int) =
      Optional[List[A], A](
        (from) => Try(from(i)).toOption,
        (to) => (from) => from.updated(i, to)
      )
  }

  given [K, V]: Index[Map[K, V], K] with {
    type To = V

    def index(k: K) =
      Optional[Map[K, V], V](
        (from) => from.get(k),
        to => from => from.updated(k, to)
      )
  }
}
