package optics.poly
import optics.internal.focus.FocusImpl


object dsl {
  extension [A, B] (a: Option[A])
    def ? : B = ???

  extension [A, K, B] (a: A)
    def idx(k: K)(using Index[A, K] { type To = B}): B = ???
}


object Focus {

  def apply[S] = new MkFocus[S]

  class MkFocus[S] {
    inline def apply[T](inline get: (S => T)): Lens[S,T] = 
      ${ FocusImpl('get) }
  }
}
