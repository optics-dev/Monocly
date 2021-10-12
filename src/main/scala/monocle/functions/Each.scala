package monocle.functions

import monocle._

abstract class Each[S, A]:
  def each: Optic[GetMany & Modify, S, A]
