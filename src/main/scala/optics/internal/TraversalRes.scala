package optics.internal

case class TraversalRes[F[+ _], +E, +A](optError: Option[E], effect: F[A]){
  def flatten[G[+_], E1 >: E, B](implicit ev: F[A] <:< TraversalRes[G, E1, B]): TraversalRes[G, E1, B] = {
    val self = ev(effect)
    TraversalRes(
      optError = optError.orElse(self.optError),
      effect   = self.effect
    )
  }
}

object TraversalRes {
  implicit def applicative[F[+_]: Applicative, E]: Applicative[[+X] =>> TraversalRes[F, E, X]] =
    new Applicative[[+X] =>> TraversalRes[F, E, X]]{
      val F: Applicative[F] = Applicative[F]

      def pure[A](value: A): TraversalRes[F, E, A] = TraversalRes(None, F.pure(value))

      def map2[A, B, C](fa: TraversalRes[F, E, A], fb: TraversalRes[F, E, B])(f: (A, B) => C): TraversalRes[F, E, C] =
        TraversalRes(
          optError = fa.optError.orElse(fb.optError),
          effect   = F.map2(fa.effect, fb.effect)(f)
        )

      def map[A, B](fa: TraversalRes[F, E, A])(f: A => B): TraversalRes[F, E, B] =
        TraversalRes(fa.optError, F.map(fa.effect)(f))
    }
}
