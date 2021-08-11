package optics.poly.functions

import optics.poly._


abstract class Index[S, -I, A]:
  def index(i: I): Optic[GetOption & Modify, S, A]
