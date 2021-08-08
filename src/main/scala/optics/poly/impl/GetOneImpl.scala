package optics.poly


class GetOneImpl[+ThisCan <: GetOne, -S, +A](val get: S => A) extends GetterImpl[ThisCan, S, A]: 
  self => 

  final def getOption: S => Option[A] = 
    s => Some(get(s))

  final def getAll: S => List[A] = 
    s => List(get(s))

  override def preComposeGetMany[ThatCan <: GetMany, S0](impl1: GetManyImpl[ThatCan, S0, S]): GetManyImpl[ThisCan | ThatCan, S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).map(get))

  // override def preComposeGetOneOrMore[S0](impl1: GetOneOrMoreImpl[S0,S]): GetOneOrMoreImpl[S0, A] = 
  //   GetOneOrMoreImpl(s0 => impl1.getOneOrMore(s0).map(get))

  override def preComposeGetOption[ThatCan <: GetOption, S0](impl1: GetOptionImpl[ThatCan, S0, S]): GetOptionImpl[ThisCan | ThatCan, S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).map(get))

  override def preComposeGetOne[ThatCan <: GetOne, S0](impl1: GetOneImpl[ThatCan, S0, S]): GetOneImpl[ThisCan | ThatCan, S0, A] = 
    GetOneImpl(s0 => get(impl1.get(s0)))

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[ThisCan | ThatCan, S, C] = 
    impl2.preComposeGetOne(this)

  override def doGet(using ThisCan <:< GetOne): S => A = get
  override def doGetOption(using ThisCan <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using ThisCan <:< GetMany): S => List[A] = getAll

end GetOneImpl