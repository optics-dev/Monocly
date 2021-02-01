package optics.poly

import optics.internal.focus.FocusImpl

object Focus {
  def apply[S] = new MkFocus[S]

  class MkFocus[S] {
    inline def apply[T](inline get: (S => T)): Lens[S,T] = 
      ${ FocusImpl('get) }
  }
}
