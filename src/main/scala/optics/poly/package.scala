package optics.poly


type Traversal[From, To] = PTraversal[From, From, To, To]
type Optional[From, To]  = POptional[From, From, To, To]
type Prism[From, To]     = PPrism[From, From, To, To]
type Lens[From, To]      = PLens[From, From, To, To]
type Iso[From, To]       = PIso[From, From, To, To]
