package monocly.functions

import monocly.*
import monocly.classic.Optional

abstract class Index[S, -I, A]:
  def index(i: I): Optional[S, A]

object Index:
  def apply[I, S, A](i: I)(using idx: Index[S, I, A]): Optional[S, A] =
      idx.index(i)

  def map[K, V](key: K): Optional[Map[K, V], V] = 
    apply(key)

  given [K, V]: Index[Map[K,V], K, V] with
    def index(i: K): Optional[Map[K,V], V] = 
      Optional(_.get(i), v => _.updated(i, v))
  
  given [A]: Index[List[A], Int, A] with
    def index(i: Int): Optional[List[A], A] = 
      Optional(_.lift(i), v => _.updated(i, v))
      
end Index