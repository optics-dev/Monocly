package optics.internal

trait Monoid[A]:
  def combine(x: A, y: A): A
  def empty: A
