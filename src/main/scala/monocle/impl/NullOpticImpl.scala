package monocle.impl

private[monocle] object NullOpticImpl extends OpticImpl[Nothing, Any, Nothing, Nothing, Any]:
  override def andThen[Can2 >: Nothing, C, D](
    optic2: OpticImpl[Can2, Nothing, Any, C, D]
  ): OpticImpl[Nothing, Any, Nothing, C, D] =
    this

  override def toString: String =
    "NullOpticImpl"
