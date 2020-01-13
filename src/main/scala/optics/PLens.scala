package optics

import scala.annotation.alpha

trait PLens[-S, +T, +A, -B] extends EPOptional[Nothing, S, T, A, B] { self =>
  def get(from: S): A

  def getOrModify(from: S): Either[(Nothing, T), A] = Right(get(from))

  override def modify(f: A => B): S => T = from => replace(f(get(from)))(from)

  @alpha("andThen")
  def >>>[C, D](other: PLens[A, B, C, D]): PLens[S, T, C, D] = new PLens[S, T, C, D] {
    def get(from: S): C = other.get(self.get(from))
    def replace(to: D): S => T = self.modify(other.replace(to))
    override def modify(f: C => D): S => T = self.modify(other.modify(f))
  }
}

object PLens {
  def apply[S, T, A, B](_get: S => A)(_replace: B => S => T): PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def get(from: S): A = _get(from)
    def replace(to: B): S => T = _replace(to)
  }
}

object Lens {
  def apply[A, B](_get: A => B)(_replace: B => A => A): Lens[A, B] = PLens(_get)(_replace)
}