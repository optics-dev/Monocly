package optics.poly

import scala.annotation.alpha

trait AndThen[F[_, _, _, _, _], G[_, _, _, _, _], H[_, _, _, _, _]] {
  def andThen[E, E1, S, T, A, B, C, D](
    f: F[E, S, T, A, B],
    g: G[E1, A, B, C, D]): H[E | E1, S, T, C, D]
}


import optics.internal.{Applicative, Id, Proxy, TraversalRes}

object AndThen {

  given as AndThen[
    EPTraversal,
    EPTraversal,
    EPTraversal
  ] {
    def andThen[E, E1, S, T, A, B, C, D](
      x: EPTraversal[E, S, T, A, B],
      y: EPTraversal[E1, A, B, C, D]
    ): EPTraversal[E | E1, S, T, C, D] =
      new EPTraversal[E | E1, S, T, C, D] {
        def traversal[F[+ _] : Applicative, E2](f: C => TraversalRes[F, E2, D])(from: S): TraversalRes[F, E | E1 | E2, T] =
          x.traversal(y.traversal(f)(_))(from)
      }
  }

  given as AndThen[
    EPTraversal,
    EPPrism,
    EPTraversal
  ] {
    def andThen[E, E1, S, T, A, B, C, D](
      x: EPTraversal[E, S, T, A, B],
      y: EPPrism[E1, A, B, C, D]
    ): EPTraversal[E | E1, S, T, C, D] =
      summon[AndThen[EPTraversal, EPTraversal, EPTraversal]]
        .andThen(x, y)
  }


  given as AndThen[
    EPTraversal,
    EPOptional,
    EPTraversal
  ] {
    def andThen[E, E1, S, T, A, B, C, D](
      x: EPTraversal[E, S, T, A, B],
      y: EPOptional[E1, A, B, C, D]
    ): EPTraversal[E | E1, S, T, C, D] =
      summon[AndThen[EPTraversal, EPTraversal, EPTraversal]]
        .andThen(x, y)
  }

  given as AndThen[
    EPOptional,
    EPOptional,
    EPOptional
  ] {
    def andThen[E, E1, S, T, A, B, C, D](
      x: EPOptional[E, S, T, A, B],
      y: EPOptional[E1, A, B, C, D]
    ): EPOptional[E | E1, S, T, C, D] =
      new EPOptional[E | E1, S, T, C, D] {
        def getOrModify(from: S): Either[(E | E1, T), C] =
          for {
            a <- x.getOrModify(from)
            t <- y.getOrModify(a).left.map{ case (e1, b) => (e1, x.replace(b)(from)) }
          } yield t
        override def replace(to: D): S => T = x.modify(y.replace(to))
      }
  }

  given as AndThen[
    EPOptional,
    EPPrism,
    EPOptional
  ] {
    def andThen[E, E1, S, T, A, B, C, D](
      x: EPOptional[E, S, T, A, B],
      y: EPPrism[E1, A, B, C, D]
    ): EPOptional[E | E1, S, T, C, D] =
      summon[AndThen[EPOptional, EPOptional, EPOptional]]
        .andThen(x, y)
  }

  given as AndThen[
    EPPrism,
    EPPrism,
    EPPrism
  ] {

    def andThen[E, E1, S, T, A, B, C, D](
      x: EPPrism[E, S, T, A, B],
      y: EPPrism[E1, A, B, C, D]
    ): EPPrism[E | E1, S, T, C, D] =
      new EPPrism[E | E1, S, T, C, D] {
        def getOrModify(from: S): Either[(E | E1, T), C] =
          for {
            a <- x.getOrModify(from)
            t <- y.getOrModify(a).left.map{ case (e1, b) => (e1, x.replace(b)(from)) }
          } yield t
        def reverseGet(to: D): T = x.reverseGet(y.reverseGet(to))
      }
  }

  given as AndThen[
    EPPrism,
    EPTraversal,
    EPTraversal
  ] {
    def andThen[E, E1, S, T, A, B, C, D](
      x: EPPrism[E, S, T, A, B],
      y: EPTraversal[E1, A, B, C, D]
    ): EPTraversal[E | E1, S, T, C, D] =
      summon[AndThen[EPTraversal, EPTraversal, EPTraversal]]
        .andThen(x, y)
  }
}
