package optics.poly

import optics.internal.{Applicative, TraversalRes}

import scala.annotation.alpha
import scala.util.Try

trait EPOptional[+E, -S, +T, +A, -B] extends EPTraversal[E, S, T, A, B] { self =>
  def getOrModify(from: S): Either[(E, T), A]

  def traversal[F[+_] : Applicative, E1](f: A => TraversalRes[F, E1, B])(from: S): TraversalRes[F, E | E1, T] =
    getOrModify(from) match {
      case Left((e, t)) => TraversalRes.Failure(e, Applicative[F].pure(t))
      case Right(a)     => f(a).map(replace(_)(from))
    }

  def getOrError(from: S): Either[E, A] =
    getOrModify(from).left.map(_._1)

  def getOption(from: S): Option[A] =
    getOrModify(from).toOption

  def get(from: S)(implicit ev: E <:< Nothing): A =
    getOrModify(from).getOrElse(???)

  def mapError[E1](update: E => E1): EPOptional[E1, S, T, A, B] =
    EPOptional[E1, S, T, A, B](getOrModify(_).left.map{ case (e, t) => (update(e), t)}, b => s => replaceOrError(b)(s).left.map(update))

  def some[A1, B1](implicit ev1: A <:< Option[A1], ev2: Option[B1] <:< B): EPOptional[E | String, S, T, A1, B1] =
    adapt >>> PPrism.some

  def adapt[A1, B1](implicit evA: A <:< A1, evB: B1 <:< B): EPOptional[E, S, T, A1, B1] =
    evB.substituteContra[[X] =>> EPOptional[E, S, T, A1, X]](
      evA.substituteCo[[X] =>> EPOptional[E, S, T, X, B]](this)
    )

  // @alpha("andThen")
  // def >>>[E1, C, D](other: EPOptional[E1, A, B, C, D]): EPOptional[E | E1, S, T, C, D] = new EPOptional[E | E1, S, T, C, D] {
  //   def getOrModify(from: S): Either[(E | E1, T), C] =
  //     for {
  //       a <- self.getOrModify(from)
  //       t <- other.getOrModify(a).left.map{ case (e1, b) => (e1, self.replace(b)(from)) }
  //     } yield t
  //   override def replace(to: D): S => T = self.modify(other.replace(to))
  // }

}

object EPOptional {
  def apply[E, S, T, A, B](_getOrModify: S => Either[(E, T), A], _replaceOrError: B => S => Either[E, T]): EPOptional[E, S, T, A, B] =
    new EPOptional[E, S, T, A, B] {
      def getOrModify(from: S): Either[(E, T), A] = _getOrModify(from)
      override def replaceOrError(to: B): S => Either[E, T] = _replaceOrError(to)
    }
}

object POptional {
  def apply[S, T, A, B](_getOrModify: S => Either[T, A], _replace: B => S => T): POptional[S, T, A, B] =
    EPOptional[Any, S, T, A, B](_getOrModify(_).left.map(defaultError -> _), b => s => Right(_replace(b)(s)))
}

object EOptional {
  def apply[Error, From, To](_getOrError: From => Either[Error, To], _replaceOrError: To => From => Either[Error, From]): EOptional[Error, From, To] =
    EPOptional(from => _getOrError(from).left.map(_ -> from), _replaceOrError)

  // def indexMap[K, V](key: K): EOptional[String, Map[K, V], V] =
  //   apply[String, Map[K, V], V](
  //     _.get(key).toRight(s"Key $key is missing"),
  //     newValue => map => if(map.contains(key)) map + (key -> newValue) else map
  //   )

  // def indexList[A](key: Int): EOptional[String, List[A], A] =
  //   apply[String, List[A], A](
  //     list => Try(list(key)).toOption.toRight(s"No value at index $key"),
  //     newValue => list => Try(list.updated(key, newValue)).getOrElse(list)
  //   )
}

object Optional {
  def apply[From, To](_getOption: From => Option[To], _replace: To => From => From): Optional[From, To] =
    EOptional(_getOption(_).toRight(()), to => from => Right(_replace(to)(from)))
}
