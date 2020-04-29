package optics.poly

extension ops {
  def [
    F[_, _, _, _, _],
    G[_, _, _, _, _],
    H[_, _, _, _, _],
    E1,
    E,
    S,
    T,
    A,
    B,
    C,
    D
  ](x: F[E, S, T, A, B]) >>> (y: G[E1, A, B, C, D])(using Compose[F, G, H]): H[E | E1, S, T, C, D] = ???
}

trait Compose[F[_, _, _, _, _], G[_, _, _, _, _], H[_, _, _, _, _]] {
  def compose[E, E1, S, T, A, B, C, D](
    f: F[E, S, T, A, B],
    g: G[E1, A, B, C, D]): H[E | E1, S, T, C, D] = ???
}

object Compose {

  given as Compose[
    EPTraversal,
    EPTraversal,
    EPTraversal
  ] {

  }

  given as Compose[
    EPTraversal,
    EPPrism,
    EPTraversal
  ] {
  }


  given as Compose[
    EPTraversal,
    EPOptional,
    EPTraversal
  ] {
  }

  given as Compose[
    EPOptional,
    EPOptional,
    EPOptional
  ] {
  }

  given as Compose[
    EPOptional,
    EPPrism,
    EPOptional
  ] {
  }

  given as Compose[
    EPPrism,
    EPPrism,
    EPPrism
  ] {
  }

  given as Compose[
    EPPrism,
    EPTraversal,
    EPTraversal
  ] {
  }
}
