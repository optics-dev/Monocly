
package object optics {

  type NonEmptyTraversal[From, To] = ETraversal[Nothing, From, To]
  type Traversal[From, To]         = ETraversal[Any, From, To]
  type Optional[From, To]          = EOptional[Any, From, To]
  type Lens[From, To]              = EOptional[Nothing, From, To]
  type Prism[From, To]             = EPrism[Any, From, To]
  type Iso[From, To]               = EPrism[Nothing, From, To]

  val defaultError: Unit = ()

}
