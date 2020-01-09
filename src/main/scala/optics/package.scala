
package object optics {
  type Lens[S, A] = PLens[S, S, A, A]
  type Prism[S, A] = PPrism[S, S, A, A]
  type Optional[S, A] = POptional[S, S, A, A]
}
