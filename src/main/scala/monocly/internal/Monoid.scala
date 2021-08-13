package monocly.internal

trait Semigroup[A]: 
  def combine(x: A, y: A): A

object Semigroup:
  given [A]: Semigroup[NonEmptyList[A]] with
    def combine(x: NonEmptyList[A], y: NonEmptyList[A]): NonEmptyList[A] = x ++ y

  def apply[M](using m: Semigroup[M]) = m
end Semigroup

trait Monoid[A] extends Semigroup[A]:
  def empty: A

object Monoid: 
  given Monoid[Int] with
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int = 0

  given [A]: Monoid[List[A]] with
    def combine(x: List[A], y: List[A]): List[A] = x ++ y
    def empty: List[A] = Nil

  given Monoid[String] with
    def combine(x: String, y: String): String = x + y
    def empty: String = ""

  def apply[M](using m: Monoid[M]) = m
end Monoid
  