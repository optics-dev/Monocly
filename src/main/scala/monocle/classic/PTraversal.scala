package monocle.classic

import monocle.*
import monocle.internal.{Applicative, Id, Proxy}
import monocle.functions.Index
import monocle.impl._

type PTraversal[-S, +T, +A, -B] = FullOptic[GetMany & Modify, S, T, A, B]
type Traversal[From, To]        = PTraversal[From, From, To, To]

object PTraversal:
  def field2[S, T, A, B](get1: S => A, get2: S => A)(_replace: (B, B) => S => T): PTraversal[S, T, A, B] =
    FullOptic.thatCan.edit2(get1, get2)(_replace)

  def pair[A, B]: PTraversal[(A, A), (B, B), A, B] =
    field2[(A, A), (B, B), A, B](_._1, _._2)((b1, b2) => _ => (b1, b2))

  def list[A, B]: PTraversal[List[A], List[B], A, B] =
    FullOptic.thatCan.traverse

end PTraversal

object Traversal:
  def field2[From, To](get1: From => To, get2: From => To)(_replace: (To, To) => From => From): Traversal[From, To] =
    PTraversal.field2(get1, get2)(_replace)

  def pair[A]: Traversal[(A, A), A]  = PTraversal.pair
  def list[A]: Traversal[List[A], A] = PTraversal.list

end Traversal
