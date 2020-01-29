package optics.mono

object Iso {
  def apply[From, To](_get: From => To, _reverseGet: To => From): Iso[From, To] =
    EPrism(from => Right(_get(from)), _reverseGet)

  def id[A]: Iso[A, A] =
    apply(identity, identity)
}