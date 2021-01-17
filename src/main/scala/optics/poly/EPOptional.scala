package optics.poly

import optics.internal.{Applicative, TraversalRes}

import scala.annotation.alpha
import scala.util.Try

trait EPOptional[+E, -S, +T, +A, -B] extends EPTraversal[E, S, T, A, B] { self =>
  def getOrModify(from: S): Either[(E, T), A]

  def traversal[F[+_] : Applicative, E1](f: A => TraversalRes[F, E1, B])(from: S): TraversalRes[F, E | E1, T] =
    getOrModify(from) match {
      case Left((e, t)) => TraversalRes.Failure(e, Applicative[F].pure(t))
      case Right(a)     =>
        f(a).map(replace(_)(from))
    }

  def getOrError(from: S): Either[E, A] =
    getOrModify(from).left.map(_._1)

  def getOption(from: S): Option[A] =
    getOrModify(from).toOption

  def get(from: S)(implicit ev: E <:< Nothing): A =
    getOrModify(from).getOrElse(???)

  def mapError[E1](update: E => E1): EPOptional[E1, S, T, A, B] =
    EPOptional[E1, S, T, A, B](getOrModify(_).left.map{ case (e, t) => (update(e), t)}, replace)

  def some[A1, B1](implicit ev1: A <:< Option[A1], ev2: Option[B1] <:< B): EPOptional[E | NoSuchElementException, S, T, A1, B1] =
    adapt >>> PPrism.some

  def adapt[A1, B1](implicit evA: A <:< A1, evB: B1 <:< B): EPOptional[E, S, T, A1, B1] =
    evB.substituteContra[[X] =>> EPOptional[E, S, T, A1, X]](
      evA.substituteCo[[X] =>> EPOptional[E, S, T, X, B]](this)
    )

}



object EPOptional {
  extension [G[_, _, _, _, _], H[_, _, _, _, _],E1,E,S,T,A,B,C,D] (x: EPOptional[E, S, T, A, B]) {
    @alpha("andThen")
    def >>>(y: G[E1, A, B, C, D])(using AndThen[EPOptional, G, H]): H[E | E1, S, T, C, D] =
      summon[AndThen[EPOptional, G, H]].andThen[E, E1, S, T, A, B, C, D](x, y)
  }

  def apply[E, S, T, A, B](_getOrModify: S => Either[(E, T), A], _replace: B => S => T): EPOptional[E, S, T, A, B] =
    new EPOptional[E, S, T, A, B] {
      def getOrModify(from: S): Either[(E, T), A] = _getOrModify(from)
      override def replace(to: B): S => T = _replace(to)
      override def replaceOrError(to: B): S => Either[E, T] =
       s => _getOrModify(s).left.map(_._1).map(_ => replace(to)(s))
    }
}

object POptional {
  def apply[S, T, A, B](_getOrModify: S => Either[T, A], _replace: B => S => T): POptional[S, T, A, B] =
    EPOptional[Any, S, T, A, B](_getOrModify(_).left.map(defaultError -> _), _replace)
}

object EOptional {
  def apply[Error, From, To](_getOrError: From => Either[Error, To], _replace: To => From => From): EOptional[Error, From, To] =
    EPOptional(from => _getOrError(from).left.map(_ -> from), _replace)
}

object Optional {
  def apply[From, To](_getOption: From => Option[To], _replace: To => From => From): Optional[From, To] =
    EOptional(_getOption(_).toRight(()), _replace)
}
