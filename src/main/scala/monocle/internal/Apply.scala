package monocle.internal


trait Apply[F[_]] extends Functor[F]:
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]

object Apply: 
  def apply[F[_]](using ev: Apply[F]): Apply[F] = ev

  given const[M](using sem: Semigroup[M]): Apply[[a] =>> Const[M, a]] with 
    def map2[A, B, C](fa: Const[M, A], fb: Const[M, B])(f: (A, B) => C): Const[M, C] = Const(sem.combine(fa.getConst, fb.getConst))
    def map[A, B](fa: Const[M, A])(f: A => B): Const[M, B] = Const(fa.getConst)

  given id: Apply[Id] with
    def map2[A, B, C](fa: Id[A], fb: Id[B])(f: (A, B) => C): Id[C] = f(fa, fb)
    def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

  given proxy: Apply[Proxy] with
    def map2[A, B, C](fa: Proxy[A], fb: Proxy[B])(f: (A, B) => C): Proxy[C] = Proxy.nothing
    def map[A, B](fa: Proxy[A])(f: A => B): Proxy[B] = Proxy.nothing

end Apply
