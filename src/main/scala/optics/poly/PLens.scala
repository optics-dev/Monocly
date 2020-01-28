package optics.poly

object PLens {
  def apply[S, T, A, B](_get: S => A, _replace: B => S => T): PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def getOrModify(from: S): Either[(Nothing, T), A] = Right(_get(from))
    override def replace(to: B): S => T = _replace(to)
  }

  def _1[A1, A2, B]: PLens[(A1, A2), (B, A2), A1, B] =
    apply[(A1, A2), (B, A2), A1, B](_._1, newValue => _.copy(_1 = newValue))

  def _2[A1, A2, B]: PLens[(A1, A2), (A1, B), A2, B] =
    apply[(A1, A2), (A1, B), A2, B](_._2, newValue => _.copy(_2 = newValue))
}


object Lens {
  def apply[From, To](_get: From => To, _replace: To => From => From): Lens[From, To] =
    PLens(_get, _replace)
}