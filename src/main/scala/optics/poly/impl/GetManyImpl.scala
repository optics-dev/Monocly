package optics.poly


class GetManyImpl[-S,+A](val getAll: S => List[A]) extends GetterImpl[GetMany, S, A]: 

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  // override def preComposeGetOneOrMore[AllowedByBoth >: (GetMany | GetOneOrMore) <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0,S]): GetManyImpl[S0, A] =
  //   GetManyImpl(s0 => impl1.getOneOrMore(s0).toList.flatMap(getAll))

  override def preComposeGetOption[AllowedByBoth >: (GetMany | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0,S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => getAll(impl1.get(s0)))

  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[GetMany | ThatCan, S, C] = 
    impl2.preComposeGetMany(this)

  override def doGetAll(using GetMany <:< GetMany): S => List[A] = getAll

end GetManyImpl