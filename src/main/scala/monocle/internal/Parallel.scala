package monocle.internal



trait Parallel[M[_]]:
  type F[_]
  def sequential[A](fa: F[A]): M[A]
  def parallel[A](ma: M[A]): F[A]
  def monad: Monad[M]
  def applicative: Applicative[F]


type NonEmptyParallel[M[_]] = Parallel[M]