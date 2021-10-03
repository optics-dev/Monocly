package monocly.functions

import monocly._

abstract class Each[S, A]:
  def each: Optic[OpticCan.GetMany & OpticCan.Modify, S, A]