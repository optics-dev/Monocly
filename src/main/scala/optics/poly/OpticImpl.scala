package optics.poly

/*
type ComposedImpl[I1 <: GetImpl[ThisCan, S,A], I2 <: GetImpl[ThatCan, A,C], ThisCan, ThatCan, S, A, C] <: GetImpl[ThisCan | ThatCan, S,C] = (I1, I2) match 
  case (GetNoneImpl[S,A], x) => GetNoneImpl[S,C]
  case (x, GetNoneImpl[A,C]) => GetNoneImpl[S,C]
  case (GetManyImpl[S,A], x) => GetManyImpl[S,C]
  case (x, GetManyImpl[A,C]) => GetManyImpl[S,C]
  case (GetOptionImpl[S,A], x) => GetOptionImpl[S,C]
  case (x, GetOptionImpl[S,A]) => GetOptionImpl[S,C]
  case (GetOneImpl[S,A], GetOneImpl[A,C]) => GetOneImpl[S,C] */

sealed trait GetImpl[+ThisCan <: OpticCanGet, -S, +A]:

  def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetImpl[ThisCan | GetMany, S0, A]
  def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetImpl[ThisCan | GetOption, S0, A]
  def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetImpl[ThisCan | GetOne, S0, A]

  def andThen[ThatCan <: OpticCanGet, C](impl2: GetImpl[ThatCan, A,C]): GetImpl[ThisCan | ThatCan, S, C]

  def doGet(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def doGetOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  def doGetAll(using ThisCan <:< GetMany): S => List[A] = sys.error("This optic does not support 'getAll'")

end GetImpl

class GetNoneImpl[-S, +A] extends GetImpl[GetNone, S, A]:
  //type PreComposeGetManyImpl[-S0] = GetNoneImpl[S0, A]
  //type PreComposeGetOptionImpl[-S0] = GetNoneImpl[S0, A]
  //type PreComposeGetOneImpl[-S0] = GetNoneImpl[S0, A]

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetNoneImpl[S0, A] = new GetNoneImpl
  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0, S]): GetNoneImpl[S0, A] = new GetNoneImpl
  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetNoneImpl[S0, A] = new GetNoneImpl

  def andThen[ThatCan <: OpticCanGet, C](impl2: GetImpl[ThatCan, A, C]): GetNoneImpl[S, C] = new GetNoneImpl

end GetNoneImpl

abstract class GetManyImpl[-S,+A] extends GetImpl[GetMany, S, A]: 
  self => 
  
  def getAll: S => List[A]

  //type PreComposeGetManyImpl[-S0] = GetManyImpl[S0, A]
  //type PreComposeGetOptionImpl[-S0] = GetManyImpl[S0, A]
  //type PreComposeGetOneImpl[-S0] = GetManyImpl[S0, A]

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetManyImpl[S0, A] = 
    new GetManyImpl:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetManyImpl[S0, A] = 
    new GetManyImpl:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetManyImpl[S0, A] = 
    new GetManyImpl[S0,A]:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  def andThen[ThatCan <: OpticCanGet, C](impl2: GetImpl[ThatCan, A, C]): GetImpl[GetMany | ThatCan, S, C] = 
    impl2.preComposeGetMany(this)

  override def doGetAll(using GetMany <:< GetMany): S => List[A] = getAll

end GetManyImpl

abstract class GetOptionImpl[-S, +A] extends GetImpl[GetOption, S, A]: 
  self => 

  def getOption: S => Option[A]

  //type PreComposeGetManyImpl[-S0] = GetManyImpl[S0, A]
  //type PreComposeGetOptionImpl[-S0] = GetOptionImpl[S0, A]
  //type PreComposeGetOneImpl[-S0] = GetOptionImpl[S0, A]

  final def getAll: S => List[A] = 
    s => getOption(s).toList

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
    new GetManyImpl:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0, S]): GetOptionImpl[S0, A] = 
    new GetOptionImpl:
      def getOption: S0 => Option[A] = 
        s0 => impl1.getOption(s0).flatMap(self.getOption)

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOptionImpl[S0, A] = 
    new GetOptionImpl:
      def getOption: S0 => Option[A] = 
        s0 => impl1.getOption(s0).flatMap(self.getOption)

  def andThen[ThatCan <: OpticCanGet, C](impl2: GetImpl[ThatCan, A, C]): GetImpl[GetOption | ThatCan, S, C] = 
    impl2.preComposeGetOption(this)

  override def doGetOption(using GetOption <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using GetOption <:< GetMany): S => List[A] = getAll

end GetOptionImpl

abstract class GetOneImpl[-S, +A] extends GetImpl[GetOne, S, A]: 
  self => 

  def get: S => A

  final def getOption: S => Option[A] = 
    s => Some(get(s))

  final def getAll: S => List[A] = 
    s => List(get(s))

  //type PreComposeGetManyImpl[-S0] = GetManyImpl[S0, A]
  //type PreComposeGetOptionImpl[-S0] = GetOptionImpl[S0, A]
  //type PreComposeGetOneImpl[-S0] = GetOneImpl[S0, A]

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
    new GetManyImpl[S0,A]:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).map(self.get)

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0, S]): GetOptionImpl[S0, A] = 
    new GetOptionImpl:
      def getOption: S0 => Option[A] = 
        s0 => impl1.getOption(s0).map(self.get)

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOneImpl[S0, A] = 
    new GetOneImpl:
      def get: S0 => A = 
        s0 => self.get(impl1.get(s0))

  def andThen[ThatCan <: OpticCanGet, C](impl2: GetImpl[ThatCan, A, C]): GetImpl[GetOne | ThatCan, S, C] = 
    impl2.preComposeGetOne(this)

  override def doGet(using GetOne <:< GetOne): S => A = get
  override def doGetOption(using GetOne <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using GetOne <:< GetMany): S => List[A] = getAll

end GetOneImpl


sealed trait ReplaceImpl[-S, +T, -B]:
  def replace(b: B): S => T

trait ReplaceAllImpl[-S, +T, -B] extends ReplaceImpl[S, T, B]:
  def replaceAll: B => T
  override def replace(b: B): S => T = 
    _ => replaceAll(b)

trait ReplaceNoneImpl[S] extends ReplaceAllImpl[S,S,S]:
  final override def replaceAll: S => S = 
    s => s
  final override def replace(x: S): S => S = 
    s => s