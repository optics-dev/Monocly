package monocly.functions

import monocly._


abstract class Index[S, -I, A]:
  def index(i: I): Optic[GetOption & Modify, S, A]
