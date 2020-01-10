package optics

import scala.annotation.alpha

trait POptional[-S, +T, +A, -B] { self =>
  def getOrModify(from: S): Either[T, A]

  def set(to: B): S => T

  def getOption(from: S): Option[A] =
    getOrModify(from).toOption

  def modify(f: A => B): S => T = from =>
    getOrModify(from).fold(identity, a => set(f(a))(from))

  @alpha("andThen")
  def >>>[C, D](other: POptional[A, B, C, D]): POptional[S, T, C, D] = new POptional[S, T, C, D] {
    def getOrModify(from: S): Either[T, C] = self.getOrModify(from).flatMap(other.getOrModify(_).left.map(self.set(_)(from)))
    def set(to: D): S => T = self.modify(other.set(to))
    override def modify(f: C => D): S => T = self.modify(other.modify(f))
  }
}

object POptional {
  def apply[S, T, A, B](_getOrModify: S => Either[T, A])(_set: (B, S) => T): POptional[S, T, A, B] = new POptional[S, T, A, B] {
    def getOrModify(from: S): Either[T, A] = _getOrModify(from)
    def set(to: B): S => T = _set(to, _)
  }
}

object MOptional {
  def apply[A, B](_getOption: A => Option[B])(_set: (B, A) => A): Optional[A, B] =
    POptional[A, A, B, B](from => _getOption(from).toRight(from))(_set)
}
