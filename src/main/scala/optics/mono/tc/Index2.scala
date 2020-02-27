package optics.mono.tc

import optics.mono.Optional

import optics.mono.Optional

import scala.util.Try

trait Index2[From] {
  type Key
  type To

  def index(key: Key): Optional[From, To]

}

object Index2 {
  type Aux[From, _Key, _To] = Index2[From] { type Key = _Key; type To = _To }

  def index[From, Key, To](key: Key)(implicit ev: Aux[From, Key, To]): Optional[From, To] = ev.index(key)

  implicit def mapIndex[K, V]: Aux[Map[K, V], K, V] = new Index2[Map[K, V]]  {
    type Key = K
    type To = V
    def index(key: Key): Optional[Map[K, V], V] =
      Optional.indexMap(key)
  }

  implicit def listIndex[A]: Aux[List[A], Int, A] = new Index2[List[A]] {
    type Key = Int
    type To = A

    def index(key: Int): Optional[List[A], A] =
      if (key < 0)
        Optional.void
      else
        Optional[List[A], A](_.drop(key).headOption, a => xs => Try(xs.updated(key, a)).getOrElse(xs))
  }
}




