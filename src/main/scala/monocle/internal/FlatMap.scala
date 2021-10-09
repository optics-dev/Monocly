package monocle.internal

trait FlatMap[F[_]] extends Apply[F]:
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))

object FlatMap: 
  def apply[F[_]](using ev: FlatMap[F]): FlatMap[F] = ev

  given id: FlatMap[Id] with
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = flatMap(fa)(f)

  given list: FlatMap[List] with
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    def map[A, B](fa: List[A])(f: A => B): List[B] = flatMap(fa)(a => List(f(a)))

  given option: FlatMap[Option] with
    def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    def map[A, B](fa: Option[A])(f: A => B): Option[B] = flatMap(fa)(a => Some(f(a)))
    
end FlatMap
