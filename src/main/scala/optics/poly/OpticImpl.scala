package optics.poly


sealed trait GetImpl[-S,+A]:
  type PreComposeGetMany <: OpticCanGet
  type PreComposeGetOption <: OpticCanGet
  type PreComposeGetOne <: OpticCanGet

  type ThisCan <: OpticCanGet

  def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetterImplFor[PreComposeGetMany, S0,A]
  def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetterImplFor[PreComposeGetOption, S0,A]
  def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetterImplFor[PreComposeGetOne, S0,A]
  def andThen[C](impl2: GetImpl[A,C]): GetImpl[S, C]
end GetImpl

class GetNoneImpl[-S,+A] extends GetImpl[S,A]:
  type PreComposeGetMany = GetNone
  type PreComposeGetOption = GetNone
  type PreComposeGetOne = GetNone
  type ThisCan = GetNone

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetNoneImpl[S0,A] = new GetNoneImpl
  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetNoneImpl[S0,A] = new GetNoneImpl
  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetNoneImpl[S0,A] = new GetNoneImpl

  def andThen[C](impl2: GetImpl[A,C]): GetNoneImpl[S,C] = new GetNoneImpl

end GetNoneImpl

abstract class GetManyImpl[-S,+A] extends GetImpl[S,A]: 
  self => 
  
  def getAll: S => List[A]

  type PreComposeGetMany = GetMany
  type PreComposeGetOption = GetMany
  type PreComposeGetOne = GetMany
  type ThisCan = GetMany

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetManyImpl[S0,A] = 
    new GetManyImpl:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetManyImpl[S0,A] = 
    new GetManyImpl:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetManyImpl[S0,A] = 
    new GetManyImpl[S0,A]:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  def andThen[C](impl2: GetImpl[A,C]): GetterImplFor[impl2.PreComposeGetMany, S, C] = 
    impl2.preComposeGetMany(this)

end GetManyImpl

abstract class GetOptionImpl[-S,+A] extends GetImpl[S,A]: 
  self => 

  def getOption: S => Option[A]

  type PreComposeGetMany = ComposedGet[GetMany, GetOption]
  type PreComposeGetOption = GetOption
  type PreComposeGetOne = GetOption
  type ThisCan = GetOption

  final def getAll: S => List[A] = 
    s => getOption(s).toList

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetManyImpl[S0,A] = 
    new GetManyImpl:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).flatMap(self.getAll)

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetOptionImpl[S0,A] = 
    new GetOptionImpl:
      def getOption: S0 => Option[A] = 
        s0 => impl1.getOption(s0).flatMap(self.getOption)

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetOptionImpl[S0,A] = 
    new GetOptionImpl:
      def getOption: S0 => Option[A] = 
        s0 => impl1.getOption(s0).flatMap(self.getOption)

  override def andThen[C](impl2: GetImpl[A,C]): GetterImplFor[impl2.PreComposeGetOption, S, C] = 
    impl2.preComposeGetOption(this)

end GetOptionImpl

abstract class GetOneImpl[-S,+A] extends GetImpl[S,A]: 
  self => 

  def get: S => A

  final def getOption: S => Option[A] = 
    s => Some(get(s))

  final def getAll: S => List[A] = 
    s => List(get(s))

  type PreComposeGetMany = GetMany
  type PreComposeGetOption = GetOption
  type PreComposeGetOne = GetOne
  type ThisCan = GetOne

  override def preComposeGetMany[S0](impl1: GetManyImpl[S0,S]): GetManyImpl[S0,A] = 
    new GetManyImpl[S0,A]:
      def getAll: S0 => List[A] = 
        s0 => impl1.getAll(s0).map(self.get)

  override def preComposeGetOption[S0](impl1: GetOptionImpl[S0,S]): GetOptionImpl[S0,A] = 
    new GetOptionImpl:
      def getOption: S0 => Option[A] = 
        s0 => impl1.getOption(s0).map(self.get)

  override def preComposeGetOne[S0](impl1: GetOneImpl[S0,S]): GetOneImpl[S0,A] = 
    new GetOneImpl:
      def get: S0 => A = 
        s0 => self.get(impl1.get(s0))

  override def andThen[C](impl2: GetImpl[A,C]): GetterImplFor[impl2.PreComposeGetOne, S, C] = 
    impl2.preComposeGetOne(this)

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

/*

Null = {NoAccess, None, Irreversible}
Fold = {Read, Many, Irreversible}
Getter = {Read, One, Irreversible}
Setter = {Write, Many, Irreversible}
Traverse = {ReadWrite, Many, Irreversible}
Optional = {ReadWrite, AtMostOne, Irreversible }
Lens = {ReadWrite, One, Irreversible }
Prism = { ReadWrite, AtMostOne, Reverse}
Iso = { ReadWrite, One, Reverse }

*/

/*
trait Optic[-S, +T, +A, -B]:
  self =>
  type Access <: Optic.AccessType
  type Cardinality <: Optic.CardinalityType
  type Reversible <: true | false

  def andThen[C, D, Acc, Car](o: Optic[S, T, C, D] {type Access = Acc; type Cardinality = Car}): Optic[A, B, C, D] { type Access = LeastAccess[self.Access, Acc]; 
                                                                                                                     type}


*/