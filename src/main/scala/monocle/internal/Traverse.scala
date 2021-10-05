package monocle.internal


trait Traverse[F[_]] extends Functor[F]:
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]


object Traverse: 
  def apply[F[_]](using ev: Traverse[F]): Traverse[F] = ev

  given list: Traverse[List] with
    def map[A, B](list: List[A])(f: A => B): List[B] = 
      list.map(f)
      
    def traverse[G[_], A, B](list: List[A])(f: A => G[B])(using app: Applicative[G]): G[List[B]] = 
      list.foldRight[G[List[B]]](app.pure(Nil))((a, gList) => app.map2(f(a), gList)(_ :: _))

  // given nel: Traverse[NonEmptyList] with
  //   def traverse[G[_], A, B](nel: NonEmptyList[A])(f: A => G[B])(using app: Applicative[G]): G[NonEmptyList[B]] = 
  //     nel.tail.foldRight[G[NonEmptyList[B]]](f(nel.head))((a, gNel) => app.map2(f(a), gNel)(_ :: _))

end Traverse