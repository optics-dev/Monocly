package monocly.internal

enum TraversalRes[F[+ _], +E, +A] {
  case Success(effect: F[A])
  case Failure(error: E, effect: F[A])

  def effect: F[A]

  def map[B](f: A => B)(implicit ev: Applicative[F]): TraversalRes[F, E, B] =
    this match {
      case Failure(e, fa) => Failure(e, ev.map(fa)(f))
      case Success(fa)    => Success(ev.map(fa)(f))
    }

  def flatten[G[+_], E1 >: E, B](implicit ev: F[A] <:< TraversalRes[G, E1, B]): TraversalRes[G, E1, B] =
    this match {
      case Failure(e, x) => ev(x) match {
        case Failure(_, x) => Failure(e, x)
        case Success(x)    => Failure(e, x)
      }
      case Success(x)    => ev(x)
    }

  def map2[G[+x] >: F[x] : Applicative, E1 >: E, B, C](other: TraversalRes[G, E1, B])(f: (A, B) => C): TraversalRes[G, E1, C] =
    (this, other) match {
      case (Failure(e, fa), Failure(_, fb)) => Failure(e, Applicative[G].map2(fa, fb)(f))
      case (Failure(e, fa), Success(fb)   ) => Failure(e, Applicative[G].map2(fa, fb)(f))
      case (Success(fa)   , Failure(e, fb)) => Failure(e, Applicative[G].map2(fa, fb)(f))
      case (Success(fa)   , Success(fb)   ) => Success(Applicative[G].map2(fa, fb)(f))
    }

  def map2Permissive[G[+x] >: F[x] : Applicative, E1 >: E, B, C](other: TraversalRes[G, E1, B])(f: (A, B) => C): TraversalRes[G, E1, C] =
    (this, other) match {
      case (Failure(e, fa), Failure(_, fb)) => Failure(e, Applicative[G].map2(fa, fb)(f))
      case (Failure(e, fa), Success(fb)   ) => Success(Applicative[G].map2(fa, fb)(f))
      case (Success(fa)   , Failure(e, fb)) => Success(Applicative[G].map2(fa, fb)(f))
      case (Success(fa)   , Success(fb)   ) => Success(Applicative[G].map2(fa, fb)(f))
    }

}

object TraversalRes {
  def pure[F[+_]: Applicative, A](value: A): TraversalRes[F, Nothing, A] =
    TraversalRes.Success(Applicative[F].pure(value))

  implicit def applicative[F[+_]: Applicative, E]: Applicative[[X] =>> TraversalRes[F, E, X]] =
    new Applicative[[X] =>> TraversalRes[F, E, X]]{
      def pure[A](value: A): TraversalRes[F, E, A] =
        TraversalRes.pure(value)

      def map2[A, B, C](fa: TraversalRes[F, E, A], fb: TraversalRes[F, E, B])(f: (A, B) => C): TraversalRes[F, E, C] =
        fa.map2(fb)(f)

      def map[A, B](fa: TraversalRes[F, E, A])(f: A => B): TraversalRes[F, E, B] =
        fa.map(f)
    }
}
