package monocle.internal


trait NonEmptyParallel[M[+_]]:
  type F[+_]
  def sequential[A](fa: F[A]): M[A]
  def parallel[A](ma: M[A]): F[A]
  def flatMap: FlatMap[M]
  def apply: Apply[F]
  

trait Parallel[M[+_]] extends NonEmptyParallel[M]:
  type F[+_]
  def sequential[A](fa: F[A]): M[A]
  def parallel[A](ma: M[A]): F[A]
  def monad: Monad[M]
  def applicative: Applicative[F]
  override def flatMap: FlatMap[M] = monad
  override def apply: Apply[F] = applicative

