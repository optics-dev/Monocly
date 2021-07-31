package optics.internal

type Id[A] = A

trait Applicative[F[_]]:
  def pure[A](value: A): F[A]
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
  def map[A, B](fa: F[A])(f: A => B): F[B]


object Applicative: 
  def apply[F[_]](using ev: Applicative[F]): Applicative[F] = ev

  implicit val id: Applicative[Id] = new Applicative[Id]:
    def pure[A](value: A): Id[A] = value
    def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

  implicit val proxy: Applicative[Proxy] = new Applicative[Proxy]:
    def pure[A](value: A): Proxy[A] = Proxy.nothing
    def map2[A, B, C](fa: Proxy[A], fb: Proxy[B])(f: (A, B) => C): Proxy[C] = Proxy.nothing
    def map[A, B](fa: Proxy[A])(f: A => B): Proxy[B] = Proxy.nothing

end Applicative
