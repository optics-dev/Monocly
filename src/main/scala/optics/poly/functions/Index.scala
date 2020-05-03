package optics.poly.functions
import optics.poly.{Iso, Lens, EOptional}

import scala.annotation.implicitNotFound
import scala.collection.immutable.{ListMap, SortedMap, Map}

trait Index[From, -Key] {

  type Error
  type To

  def index(i: Key): EOptional[Error, From, To]

}

object Index {

  def apply[Key, From, Err, T](key: Key)(using idx: Index[From, Key] { type Error = Err; type To = T}): EOptional[Err, From, T] =
    idx.index(key)

  def map[K, V](key: K): EOptional[NoSuchElementException, Map[K, V], V] =
    apply(key)

  def list[A](i: Int): EOptional[IndexOutOfBoundsException, List[A], A] =
    apply(i)

  def withError[Key, E1, From, T](key: Key, error: E1)(using idx: Index[From, Key] { type To = T}): EOptional[E1, From, T] =
    idx.index(key).mapError(_ => error)


  given [A] as Index[List[A], Int] {
    type Error = IndexOutOfBoundsException
    type To = A

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

  given [K, V] as Index[Map[K, V], K] {

    type Error = NoSuchElementException
    type To = V

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
