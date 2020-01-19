package optics

import optics.internal.{Applicative, TraversalRes}
import optics.poly.EPTraversal

import scala.annotation.alpha

trait ETraversal[+Error, From, To] extends EPTraversal[Error, From, From, To, To] { self =>
  @alpha("andThen")
  def >>>[NewError >: Error, Next](other: ETraversal[NewError, To, Next]): ETraversal[NewError, From, Next] =
    new ETraversal[NewError, From, Next] {
      def traversal[F[+ _] : Applicative](f: Next => F[Next])(from: From): TraversalRes[F, NewError, From] =
        self.traversal[[+X] =>> TraversalRes[F, NewError, X]](other.traversal(f)(_))(from).flatten
    }
}

object NonEmptyTraversal {
  def field2[From, To](get1: From => To, get2: From => To)(_replace: (To, To) => From => From): NonEmptyTraversal[From, To] =
    new NonEmptyTraversal[From, To] {
      def traversal[F[+ _] : Applicative](f: To => F[To])(from: From): TraversalRes[F, Nothing, From] = ???
    }
}
