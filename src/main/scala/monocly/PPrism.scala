package monocly

import monocly.internal.Applicative
import monocly.functions.Index
import monocly.impl._

type PPrism[-S, +T, +A, -B] = POptic[GetOption & ReverseGet, S, T, A, B]
type Prism[S, A] = PPrism[S, S, A, A] 


object PPrism:
  def apply[S, T, A, B](_getOrModify: S => Either[T, A])(_reverseGet: B => T): PPrism[S, T, A, B] =
    POptic(
      GetOptionImpl(s => _getOrModify(s).toOption), 
      ReverseGetImpl(f => s => _getOrModify(s).fold(identity, a => _reverseGet(f(a))), _reverseGet))
  
  export std.option.{pSome, some}


object Prism:
  def apply[From, To](_getOption: From => Option[To])(_reverseGet: To => From): Prism[From, To] =
    PPrism[From, From, To, To](from => _getOption(from).toRight(from))(_reverseGet)

  def partial[From, To](get: PartialFunction[From, To])(reverseGet: To => From): Prism[From, To] =
    apply(from => get.lift(from))(reverseGet)

end Prism
