package monocly

import monocly.internal.Applicative
import monocly.functions.Index
import monocly.impl._

type PLens[-S, +T, +A, -B] = POptic[GetOne & Modify, S, T, A, B]
type Lens[S, A] = PLens[S, S, A, A]

object PLens:

  def apply[S, T, A, B](_get: S => A, _replace: B => S => T): PLens[S, T, A, B] = 
    POptic(GetOneImpl(_get), ModifyImpl(f => s => _replace(f(_get(s)))(s)))

  def _1[A1, A2, B]: PLens[(A1, A2), (B, A2), A1, B] =
    apply[(A1, A2), (B, A2), A1, B](_._1, newValue => _.copy(_1 = newValue))

  def _2[A1, A2, B]: PLens[(A1, A2), (A1, B), A2, B] =
    apply[(A1, A2), (A1, B), A2, B](_._2, newValue => _.copy(_2 = newValue))

end PLens


object Lens:

  def id[A]: Lens[A,A] = apply(a => a, _ => a => a)

  def apply[From, To](_get: From => To, _replace: To => From => From): Lens[From, To] =
    PLens(_get, _replace)

  def _1[A1, A2]: Lens[(A1, A2), A1] =
    PLens._1

  def _2[A1, A2]: Lens[(A1, A2), A2] =
    PLens._2
    
end Lens