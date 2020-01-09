package optics

trait PPrism[-S, +T, +A, -B] extends POptional[S, T, A, B] { self =>
  def reverseGet(to: B): T

  def set(to: B): S => T = _ => reverseGet(to)

  override def modify(f: A => B): S => T = from =>
    getOrModify(from).fold(identity, a => reverseGet(f(a)))

  def andThen[C, D](other: PPrism[A, B, C, D]): PPrism[S, T, C, D] = new PPrism[S, T, C, D] {
    def getOrModify(from: S): Either[T, C] = self.getOrModify(from).flatMap(other.getOrModify(_).left.map(self.set(_)(from)))
    def reverseGet(to: D): T = self.reverseGet(other.reverseGet(to))
  }
}

object PPrism {
  def apply[S, T, A, B](_getOrModify: S => Either[T, A])(_reverseGet: B => T): PPrism[S, T, A, B] = new PPrism[S, T, A, B] {
    def getOrModify(from: S): Either[T, A] = _getOrModify(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def some[A, B]: PPrism[Option[A], Option[B], A, B] =
    apply[Option[A], Option[B], A, B](_.toRight(None))(Some(_))
}

object Prism {
  def apply[A, B](_getOption: A => Option[B])(_reverseGet: B => A): Prism[A, B] =
    PPrism[A, A, B, B](from => _getOption(from).toRight(from))(_reverseGet)

  def partial[A, B](get: PartialFunction[A, B])(reverseGet: B => A): Prism[A, B] =
    apply(get.lift)(reverseGet)

  def some[A]: Prism[Option[A], A] =
    PPrism.some[A, A]
}