package optics

package object poly {

  type NonEmptyPTraversal[-S, +T, +A, -B] = EPTraversal[Nothing, S, T, A, B]
  type PTraversal[-S, +T, +A, -B]         = EPTraversal[BasicError, S, T, A, B]
  type POptional[-S, +T, +A, -B]          = EPOptional[BasicError, S, T, A, B]
  type PPrism[-S, +T, +A, -B]             = EPPrism[BasicError, S, T, A, B]

  type ETraversal[+E, S, A] = EPTraversal[E, S, S, A, A]
  type EOptional[+E, S, A] = EPOptional[E, S, S, A, A]
  type EPrism[+E, S, A]    = EPPrism[E, S, S, A, A]

  type PLens[-S, +T, +A, -B] = EPOptional[Nothing, S, T, A, B]
  type PIso[-S, +T, +A, -B]  = EPPrism[Nothing, S, T, A, B]

}
