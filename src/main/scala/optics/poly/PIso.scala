package optics.poly

object PIso {
  def apply[S, T, A, B](_get: S => A, _reverseGet: B => T): PIso[S, T, A, B] = new PIso[S, T, A, B] {
    def getOrModify(from: S): Either[(Nothing, T), A] = Right(_get(from))
    def reverseGet(to: B): T = _reverseGet(to)
  }

  def id[A, B]: PIso[A, B, A, B] =
    apply[A, B, A, B](identity, identity)
}

object Iso {
  def apply[From, To](_get: From => To, _reverseGet: To => From): Iso[From, To] =
    PIso(_get, _reverseGet)

  def id[A]: Iso[A, A] =
    PIso.id
}