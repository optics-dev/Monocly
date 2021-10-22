package monocle.impl

private[monocle] object NullOpticImpl extends OpticImpl[Nothing, Any, Nothing, Nothing, Any]:
  override def andThen[Can2 >: Nothing, Out2, In2](
    optic2: OpticImpl[Can2, Nothing, Any, Out2, In2]
  ): OpticImpl[Nothing, Any, Nothing, Out2, In2] =
    this

  override def toString: String =
    "NullOpticImpl"
