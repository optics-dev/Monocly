package optics.poly

object NoGetter extends GetterImpl[Nothing, Any, Nothing]:
  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, Any]) = NoGetter
  //override def preComposeGetOneOrMore[AllowedByBoth >: (Nothing | GetOneOrMore) <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0, Any]) = NoGetter
  override def preComposeGetOption[AllowedByBoth >: (Nothing | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0, Any]) = NoGetter
  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, Any]) = NoGetter
  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, Nothing, C]) = NoGetter
end NoGetter
