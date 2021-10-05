package monocle.internal

trait Monad[F[_]] extends Applicative[F]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  final def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
  final def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

object Monad: 
  def apply[F[_]](using ev: Monad[F]): Monad[F] = ev

  given id: Monad[Id] with
    def pure[A](value: A): Id[A] = value
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

  given list: Monad[List] with
    def pure[A](value: A): List[A] = List(value)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)

  given option: Monad[Option] with
    def pure[A](value: A): Option[A] = Some(value)
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    
end Monad
