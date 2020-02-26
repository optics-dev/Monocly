package optics

package object poly {

  type PTraversal[-S, +T, +A, -B]         = EPTraversal[Any, S, T, A, B]
  type NonEmptyPTraversal[-S, +T, +A, -B] = EPTraversal[Nothing, S, T, A, B]
  type POptional[-S, +T, +A, -B]          = EPOptional[Any, S, T, A, B]
  type PPrism[-S, +T, +A, -B]             = EPPrism[Any, S, T, A, B]
  type PLens[-S, +T, +A, -B]              = EPOptional[Nothing, S, T, A, B]
  type PIso[-S, +T, +A, -B]               = EPPrism[Nothing, S, T, A, B]

  type ETraversal[+Error, From, To] = EPTraversal[Error, From, From, To, To]
  type EOptional[+Error, From, To]  = EPOptional[Error, From, From, To, To]
  type EPrism[+Error, From, To]     = EPPrism[Error, From, From, To, To]

  type NonEmptyTraversal[From, To] = NonEmptyPTraversal[From, From, To, To]
  type Traversal[From, To]         = NonEmptyPTraversal[Any, From, To, To]
  type Optional[From, To]          = EOptional[Any, From, To]
  type Prism[From, To]             = EPrism[Any, From, To]
  type Lens[From, To]              = PLens[From, From, To, To]
  type Iso[From, To]               = PIso[From, From, To, To]

  val defaultError: Unit = ()

}
