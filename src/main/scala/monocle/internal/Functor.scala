package monocle.internal


trait Functor[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]


object Functor: 
  def apply[F[_]](using ev: Functor[F]): Functor[F] = ev

  given id: Functor[Id] with
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

end Functor