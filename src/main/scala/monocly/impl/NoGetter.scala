package monocly.impl

import monocly._

object NoGetter extends GetterImpl[Nothing, Any, Nothing]:
  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, Any]) = NoGetter
  override def preComposeGetOneOrMore[ThatCan <: GetOneOrMore, S0](impl1: GetOneOrMoreImpl[ThatCan, S0, Any]) = NoGetter
  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, Any]) = NoGetter
  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, Any]) = NoGetter
  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, Nothing, C]) = NoGetter
end NoGetter
