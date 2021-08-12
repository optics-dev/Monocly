package monocly.functions

import monocly._

abstract class Each[S, A]:
  def each: Optic[GetMany & Modify, S, A]