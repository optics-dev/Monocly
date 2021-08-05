package optics.poly

import optics.internal.NonEmptyList

sealed trait GetterImpl[+ThisCan <: OpticCan, -S, +A]:

  def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetterImpl[ThisCan | GetMany, S0, A]
  //def preComposeGetOneOrMore[AllowedByBoth >: (ThisCan | GetOneOrMore) <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0,S]): GetterImpl[AllowedByBoth, S0, A]
  def preComposeGetOption[AllowedByBoth >: (ThisCan | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0,S]): GetterImpl[AllowedByBoth, S0, A]
  def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetterImpl[ThisCan | GetOne, S0, A]

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A,C]): GetterImpl[ThisCan | ThatCan, S, C]

  def doGet(using ThisCan <:< GetOne): S => A = sys.error("This optic does not support 'get'")
  def doGetOption(using ThisCan <:< GetOption): S => Option[A] = sys.error("This optic does not support 'getOption'")
  //def doGetOneOrMore(using ThisCan <:< GetOneOrMore): S => NonEmptyList[A] = sys.error("This optic does not support 'getOneOrMore'")
  def doGetAll(using ThisCan <:< GetMany): S => List[A] = sys.error("This optic does not support 'getAll'")

end GetterImpl


object NoGetter extends GetterImpl[Nothing, Any, Nothing]:
  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, Any]) = NoGetter
  //override def preComposeGetOneOrMore[AllowedByBoth >: (Nothing | GetOneOrMore) <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0, Any]) = NoGetter
  override def preComposeGetOption[AllowedByBoth >: (Nothing | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0, Any]) = NoGetter
  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, Any]) = NoGetter
  override def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, Nothing, C]) = NoGetter
end NoGetter


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

// class GetOneOrMoreImpl[-S, +A](val getOneOrMore: S => NonEmptyList[A]) extends GetterImpl[GetOneOrMore, S, A]: 

//   final def getAll: S => List[A] = 
//     s => getOneOrMore(s).toList

//   override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
//     GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

//   override def preComposeGetOneOrMore[AllowedByBoth >: GetOneOrMore <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0,S]): GetOneOrMoreImpl[S0, A] = 
//     GetOneOrMoreImpl(s0 => impl1.getOneOrMore(s0).flatMap(getOneOrMore))

//   override def preComposeGetOption[AllowedByBoth >: (GetOneOrMore | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0, S]): GetManyImpl[S0, A] = 
//     GetManyImpl(s0 => impl1.getOption(s0).fold(Nil)(s => getOneOrMore(s).toList))

//   override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOneOrMoreImpl[S0, A] = 
//     GetOneOrMoreImpl(s0 => getOneOrMore(impl1.get(s0)))

//   def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[GetOneOrMore | ThatCan, S, C] = 
//     impl2.preComposeGetOneOrMore(this)

//   override def doGetOneOrMore(using GetOneOrMore <:< GetOneOrMore): S => NonEmptyList[A] = getOneOrMore
//   override def doGetAll(using GetOneOrMore <:< GetMany): S => List[A] = getAll

// end GetOneOrMoreImpl


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


class GetOneImpl[-S, +A](val get: S => A) extends GetterImpl[GetOne, S, A]: 
  self => 

  final def getOption: S => Option[A] = 
    s => Some(get(s))

  final def getAll: S => List[A] = 
    s => List(get(s))

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
    GetManyImpl(s0 => impl1.getAll(s0).map(get))

  // override def preComposeGetOneOrMore[S0](impl1: GetOneOrMoreImpl[S0,S]): GetOneOrMoreImpl[S0, A] = 
  //   GetOneOrMoreImpl(s0 => impl1.getOneOrMore(s0).map(get))

  override def preComposeGetOption[AllowedByBoth >: (GetOne | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0, S]): GetOptionImpl[S0, A] = 
    GetOptionImpl(s0 => impl1.getOption(s0).map(get))

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOneImpl[S0, A] = 
    GetOneImpl(s0 => get(impl1.get(s0)))

  def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[GetOne | ThatCan, S, C] = 
    impl2.preComposeGetOne(this)

  override def doGet(using GetOne <:< GetOne): S => A = get
  override def doGetOption(using GetOne <:< GetOption): S => Option[A] = getOption
  override def doGetAll(using GetOne <:< GetMany): S => List[A] = getAll

end GetOneImpl