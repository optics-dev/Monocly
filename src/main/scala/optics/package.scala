
package object optics {

  type BasicError = String

  type NonEmptyTraversal[From, To] = ETraversal[Nothing, From, To]
  type Traversal[From, To]         = ETraversal[BasicError, From, To]
  type Optional[From, To]          = EOptional[BasicError, From, To]
  type Lens[From, To]              = EOptional[Nothing, From, To]
  type Prism[From, To]             = EPrism[BasicError, From, To]
  type Iso[From, To]               = EPrism[Nothing, From, To]

}
