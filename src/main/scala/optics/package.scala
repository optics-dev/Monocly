
package object optics {
  type PTraversal[S, T, A, B] = EPTraversal[Nothing, S, T, A, B]
  type POptional[S, T, A, B]  = EPOptional[Unit, S, T, A, B]
  type PPrism[S, T, A, B]     = EPPrism[Unit, S, T, A, B]

  type ETraversal[E, S, A] = EPTraversal[E, S, S, A, A]
  type EOptional[E, S, A] = EPOptional[E, S, S, A, A]
  type EPrism[E, S, A]    = EPPrism[E, S, S, A, A]

  type Traversal[S, A] = PTraversal[S, S, A, A]
  type Optional[S, A]  = POptional[S, S, A, A]
  type Prism[S, A]     = PPrism[S, S, A, A]
  type Lens[S, A]      = PLens[S, S, A, A]
}
