package optics

object Lens {
  def apply[From, To](_get: From => To, _replace: To => From => From): Lens[From, To] =
    EOptional(from => Right(_get(from)), _replace)
}
