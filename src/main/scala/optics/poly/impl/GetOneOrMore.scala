package optics.poly

import optics.internal.NonEmptyList

// class GetOneOrMoreImpl[-S, +A](val getOneOrMore: S => NonEmptyList[A]) extends GetterImpl[GetOneOrMore, S, A]: 

//   final def getAll: S => List[A] = 
//     s => getOneOrMore(s).toList

//   override def preComposeGetMany[S0](impl1: GetManyImpl[S0, S]): GetManyImpl[S0, A] = 
//     GetManyImpl(s0 => impl1.getAll(s0).flatMap(getAll))

//   // override def preComposeGetOneOrMore[AllowedByBoth >: GetOneOrMore <: OpticCan, S0](impl1: GetOneOrMoreImpl[S0,S]): GetOneOrMoreImpl[S0, A] = 
//   //   GetOneOrMoreImpl(s0 => impl1.getOneOrMore(s0).flatMap(getOneOrMore))

//   override def preComposeGetOption[AllowedByBoth >: (GetOneOrMore | GetOption) <: OpticCan, S0](impl1: GetOptionImpl[S0, S]): GetManyImpl[S0, A] = 
//     GetManyImpl(s0 => impl1.getOption(s0).fold(Nil)(s => getOneOrMore(s).toList))

//   override def preComposeGetOne[S0](impl1: GetOneImpl[S0, S]): GetOneOrMoreImpl[S0, A] = 
//     GetOneOrMoreImpl(s0 => getOneOrMore(impl1.get(s0)))

//   def andThen[ThatCan <: OpticCan, C](impl2: GetterImpl[ThatCan, A, C]): GetterImpl[GetOneOrMore | ThatCan, S, C] = 
//     ??? // impl2.preComposeGetOneOrMore(this)

//   //override def doGetOneOrMore(using GetOneOrMore <:< GetOneOrMore): S => NonEmptyList[A] = getOneOrMore
//   override def doGetAll(using GetOneOrMore <:< GetMany): S => List[A] = getAll

// end GetOneOrMoreImpl
