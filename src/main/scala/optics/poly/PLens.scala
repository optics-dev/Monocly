package optics.poly

object PLens {
  def apply[S, T, A, B](_get: S => A)(_replace: B => S => T): PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def getOrModify(from: S): Either[(Nothing, T), A] = Right(_get(from))
    override def replace(to: B): S => T = _replace(to)
  }
}

