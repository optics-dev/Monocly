package optics.poly

sealed trait GetImpl[+ThisCan <: OpticCan, -S, +A]:

  def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetImpl[ThisCan | GetMany, S0, A]
  def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetImpl[ThisCan | GetOption, S0, A]
  def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetImpl[ThisCan | GetOne, S0, A]

  def andThen[ThatCan <: OpticCan, C](impl2: GetImpl[ThatCan, A,C]): GetImpl[ThisCan | ThatCan, S, C]

  def doGet(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def doGetOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  def doGetAll(using ThisCan <:< GetMany): S => List[A] = sys.error("This optic does not support 'getAll'")

end GetImpl


object GetNoneImpl extends GetImpl[Nothing, Any, Nothing]:
  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, Any]) = GetNoneImpl
  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0, Any]) = GetNoneImpl
  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, Any]) = GetNoneImpl
  def andThen[ThatCan <: OpticCan, C](impl2: GetImpl[ThatCan, Nothing, C]) = GetNoneImpl
end GetNoneImpl


class GetManyImpl[-S,+A](val getAll: S => List[A]) extends GetImpl[GetMany, S, A]: 

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  def andThen[ThatCan <: OpticCan, C](impl2: GetImpl[ThatCan, A, C]): GetImpl[GetMany | ThatCan, S, C] = 
    impl2.preComposeGetMany(this)

  override def doGetAll(using GetMany <:< GetMany): S => List[A] = getAll

end GetManyImpl


class GetOptionImpl[-S, +A](val getOption: S => Option[A]) extends GetImpl[GetOption, S, A]: 

  final def getAll: S => List[A] = 
    s => getOption(s).toList

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0, S]): GetOptionImpl[S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOptionImpl[S0, A] = 
    new GetOptionImpl(s0 => impl1.getOption(s0).flatMap(getOption))

  def andThen[ThatCan <: OpticCan, C](impl2: GetImpl[ThatCan, A, C]): GetImpl[GetOption | ThatCan, S, C] = 
    impl2.preComposeGetOption(this)

  override def doGetOption(using GetOption <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using GetOption <:< GetMany): S => List[A] = getAll

end GetOptionImpl


class GetOneImpl[-S, +A](val get: S => A) extends GetImpl[GetOne, S, A]: 
  self => 

  final def getOption: S => Option[A] = 
    s => Some(get(s))

  final def getAll: S => List[A] = 
    s => List(get(s))

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).map(get))

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0, S]): GetOptionImpl[S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).map(get))

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOneImpl[S0, A] = 
    GetOneImpl(s0 => get(impl1.get(s0)))

  def andThen[ThatCan <: OpticCan, C](impl2: GetImpl[ThatCan, A, C]): GetImpl[GetOne | ThatCan, S, C] = 
    impl2.preComposeGetOne(this)

  override def doGet(using GetOne <:< GetOne): S => A = get
  override def doGetOption(using GetOne <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using GetOne <:< GetMany): S => List[A] = getAll

end GetOneImpl