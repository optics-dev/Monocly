package monocly.internal

trait NonEmptyTraverse[F[_]] extends Traverse[F]:
  def nonEmptyTraverse[G[_]: Apply, A, B](fa: F[A])(f: A => G[B]): G[F[B]]

object NonEmptyTraverse: 
  def apply[F[_]](using ev: NonEmptyTraverse[F]): NonEmptyTraverse[F] = ev

  given nel: NonEmptyTraverse[NonEmptyList] with
    def map[A, B](list: NonEmptyList[A])(f: A => B): NonEmptyList[B] = 
      list.map(f)

    def traverse[G[_]: Applicative, A, B](nel: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] = 
      nonEmptyTraverse(nel)(f)

    def nonEmptyTraverse[G[_]: Apply, A, B](nel: NonEmptyList[A])(f: A => G[B]): G[NonEmptyList[B]] = 
      val head: G[NonEmptyList[B]] = Apply[G].map(f(nel.head))(b => NonEmptyList(b, Nil))
      nel.tail.foldRight(head)((a, gList) => Apply[G].map2(f(a), gList)(_ :: _))
  end nel