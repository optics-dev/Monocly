package optics

import scala.annotation.alpha

trait PLens[-S, +T, +A, -B] extends POptional[S, T, A, B] { self =>
  def get(from: S): A

  def getOrModify(from: S): Either[T, A] = Right(get(from))

  override def modify(f: A => B): S => T = from => set(f(get(from)))(from)

  @alpha("andThen")
  def >>>[C, D](other: PLens[A, B, C, D]): PLens[S, T, C, D] = new PLens[S, T, C, D] {
    def get(from: S): C = other.get(self.get(from))
    def set(to: D): S => T = self.modify(other.set(to))
    override def modify(f: C => D): S => T = self.modify(other.modify(f))
  }
}

object PLens {
  def apply[S, T, A, B](_get: S => A)(_set: (B, S) => T): PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def get(from: S): A = _get(from)
    def set(to: B): S => T = _set(to, _)
  }
}

object Lens {
  def apply[A, B](_get: A => B)(_set: (B, A) => A): Lens[A, B] = PLens(_get)(_set)
}