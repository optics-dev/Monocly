package monocly

import monocly._
import monocly.impl._
import monocly.internal.Applicative
import monocly.functions.Index

type PIso[-S, +T, +A, -B] = POptic[GetOne & ReverseGet, S, T, A, B]
type Iso[S, A] = PIso[S, S, A, A]

object PIso:

  def apply[S, T, A, B](_get: S => A, _reverseGet: B => T): PIso[S, T, A, B] = 
    POptic(GetOneImpl(_get), ReverseGetImpl(f => s => _reverseGet(f(_get(s))), _reverseGet))

  def id[A, B]: PIso[A, B, A, B] =
    apply[A, B, A, B](identity, identity)

end PIso

object Iso:
  def apply[S, A](_get: S => A, _reverseGet: A => S): Iso[S, A] =
    PIso(_get, _reverseGet)

  def id[A]: Iso[A, A] =
    PIso.id