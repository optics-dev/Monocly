package monocle.internal

type Id[A] = A

trait Applicative[F[_]] extends Apply[F]:
  def pure[A](value: A): F[A]

object Applicative:
  def apply[F[_]](using ev: Applicative[F]): Applicative[F] = ev

  given const[M](using mon: Monoid[M]): Applicative[[a] =>> Const[M, a]] with
    def pure[A](value: A): Const[M, A] = Const(mon.empty)
    def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = Const(
      mon.combine(fa.getConst, fb.getConst)
    )
    def map[A, B](fa: Const[M, A])(f: A => B): Const[M, B] = Const(fa.getConst)

  given id: Applicative[Id] with
    def pure[A](value: A): Id[A]                                   = value
    def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
    def map[A, B](fa: Id[A])(f: A => B): Id[B]                     = f(fa)

  given proxy: Applicative[Proxy] with
    def pure[A](value: A): Proxy[A]                                         = Proxy.nothing
    def map2[A, B, C](fa: Proxy[A], fb: Proxy[B])(f: (A, B) => C): Proxy[C] = Proxy.nothing
    def map[A, B](fa: Proxy[A])(f: A => B): Proxy[B]                        = Proxy.nothing

end Applicative
