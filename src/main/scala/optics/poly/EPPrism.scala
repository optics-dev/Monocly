package optics.poly

import scala.annotation.alpha

trait EPPrism[+E, -S, +T, +A, -B] extends EPOptional[E, S, T, A, B] { self =>
  def reverseGet(to: B): T

  override def replace(to: B): S => T =
    _ => reverseGet(to)

  override def mapError[E1](update: E => E1): EPPrism[E1, S, T, A, B] =
    EPPrism[E1, S, T, A, B](getOrModify(_).left.map{ case (e, t) => (update(e), t)}, reverseGet)
}


object EPPrism {
  extension [G[_, _, _, _, _], H[_, _, _, _, _],E1,E,S,T,A,B,C,D] (x: EPPrism[E, S, T, A, B]) {
    @alpha("andThen")
    def >>>(y: G[E1, A, B, C, D])(using AndThen[EPPrism, G, H]): H[E | E1, S, T, C, D] =
      summon[AndThen[EPPrism, G, H]].andThen[E, E1, S, T, A, B, C, D](x, y)
  }
  
  def apply[E, S, T, A, B](_getOrModify: S => Either[(E, T), A], _reverseGet: B => T): EPPrism[E, S, T, A, B] = new EPPrism[E, S, T, A, B] {
    def getOrModify(from: S): Either[(E, T), A] = _getOrModify(from)
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def some[E, A, B](error: E): EPPrism[E, Option[A], Option[B], A, B] =
    apply[E, Option[A], Option[B], A, B](_.toRight((error, None)), Some(_))
}

object PPrism {
  def apply[S, T, A, B](_getOrModify: S => Either[T, A], _reverseGet: B => T): PPrism[S, T, A, B] =
    EPPrism(_getOrModify(_).left.map(defaultError -> _), _reverseGet)

  def some[A, B]: EPPrism[NoSuchElementException, Option[A], Option[B], A, B] =
    EPPrism.some(NoSuchElementException("None is not a Some"))
}

object EPrism {
  def apply[Error, From, To](_getOrModify: From => Either[Error, To], _reverseGet: To => From): EPrism[Error, From, To] =
    EPPrism(from => _getOrModify(from).left.map(_ -> from), _reverseGet)
}

object Prism {
  def apply[From, To](_getOption: From => Option[To], _reverseGet: To => From): Prism[From, To] =
    EPrism(_getOption(_).toRight(defaultError), _reverseGet)
}
