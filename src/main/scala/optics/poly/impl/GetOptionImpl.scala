package optics.poly

class GetOptionImpl[-S, +A](val getOption: S => Option[A]) extends GetterImpl[GetOption, S, A]: 

  final def getAll: S => List[A] = 
    s => getOption(s).toList

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  // override def preComposeGetOneOrMore[S0](impl1: GetOneOrMoreImpl[S0,S]): GetManyImpl[S0, A] = 
  //   GetManyImpl(s0 => impl1.getOneOrMore(s0).toList.flatMap(getOption))

  override def preComposeGetOption[AllowedByBoth >: GetOption <: OpticCan, S0](impl1: GetOptionImpl[S0, S]): GetOptionImpl[S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOptionImpl[S0, A] = 
    new GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[GetOption | ThatCan, S, C] = 
    impl2.preComposeGetOption(this)

  override def doGetOption(using GetOption <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using GetOption <:< GetMany): S => List[A] = getAll

end GetOptionImpl
