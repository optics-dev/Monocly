package optics.poly.functions
import optics.poly.{Iso, Lens, EOptional}

import scala.annotation.implicitNotFound
import scala.collection.immutable.{ListMap, SortedMap, Map}

trait Index[Error, From, -Key, To] {
  def index(i: Key): EOptional[Error, From, To]
}

object Index {

  def apply[Key, Error, From, To](key: Key)(using idx: Index[Error, From, Key, To]): EOptional[Error, From, To] =
    idx.index(key)

  def map[K, V](key: K): EOptional[NoSuchElementException, Map[K, V], V] =
    apply(key)

  def list[A](i: Int): EOptional[IndexOutOfBoundsException, List[A], A] =
    apply(i)

  def withError[Key, E1, Error, From, To](key: Key, error: E1)(using idx: Index[Error, From, Key, To]): EOptional[E1, From, To] =
    idx.index(key).mapError(_ => error)


  given [A] as Index[IndexOutOfBoundsException, List[A], Int, A] {
    def index(i: Int) =
      EOptional(
        (from) => {
          try {
            Right(from(i))
          } catch {
            case err: IndexOutOfBoundsException =>
              Left(err)
          }
        },
        (to) => (from) => from.updated(i, to)
      )
  }

  given [K, V] as Index[NoSuchElementException, Map[K, V], K, V] {
    def index(k: K) =
      EOptional(
        (from) => {
          try {
            Right(from(k))
          } catch {
            case err: NoSuchElementException =>
              Left(err)
          }
        },
        (to) => (from) => from + (k -> to)
      )
  }
}
