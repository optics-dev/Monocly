package optics.poly.functions

import optics.poly._

abstract class Each[S, A]:
  def each: Optic[GetMany & Modify, S, A]