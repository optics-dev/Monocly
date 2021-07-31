package optics.poly

type Traversal[From, To] = PTraversal[From, From, To, To]
type Optional[From, To]  = POptional[From, From, To, To]
type Prism[From, To]     = PPrism[From, From, To, To]
type Lens[From, To]      = PLens[From, From, To, To]
type Iso[From, To]       = PIso[From, From, To, To]

/*
object Optic: 
  sealed trait AccessType
  trait Read extends AccessType
  trait Write extends AccessType
  trait ReadWrite extends Read with Write

  sealed trait CardinalityType
  trait One extends CardinalityType
  trait AtMostOne extends One
  trait Many extends AtMostOne

  
  type LeastAccess[A <: AccessType, B <: AccessType] = (A,B) match {
    case (Nothing, _) => Nothing
    case (_, Nothing) => Nothing
    case (Read, Write) => Nothing
    case (Write, Read) => Nothing
    case (Read, Read) => Read
    case (Write, Write) => Write
    case (ReadWrite, ReadWrite) => ReadWrite
    case (ReadWrite, a) => a
    case (a, ReadWrite) => a
  }*/
  

//import Optic._

/*
type Fold_[S,A] = GetMany[S,A]
type Traversal_[S,T,A,B] = GetMany[S,A] & Set[S,T,B]
type Optional_[S,T,A,B] = GetOption[S,A] & Set[S,T,B]
type Prism_[S,T,A,B] = GetOption[S,A] & Set[S,T,B] & ReverseGet[S, T, A, B]
type Lens_[S,T,A,B] = GetOne[S,A] & Set[S,T,B]
type Iso_[S,T,A,B] = GetOne[S,A] & Set[S,T,B] & ReverseGet[S, T, A, B]
*/


//type Optic[S,T,A,B] = GetOne[S,A] | GetMany[S,A] | GetOption[S,A] | Set[S,T,B] | ReverseGet[S,T,A,B]

// extension [S,T,A,B, O1 <: Optic[S,T,A,B]] (o1: O1)
//   def andThen[C, D, O2 <: Optic[S,T,C,D]](o2: O2): O1 & O2 = ???


sealed trait OpticCanGet
class GetNone extends OpticCanGet
class GetMany extends OpticCanGet
class GetOption extends OpticCanGet
class GetOne extends OpticCanGet

sealed trait OpticCanSet
class ReplaceNone extends OpticCanSet
class Replace extends OpticCanSet
class ReplaceAll extends OpticCanSet

type ComposedGet[C1 <: OpticCanGet, C2 <: OpticCanGet] <: OpticCanGet = (C1, C2) match
  case (GetNone, GetNone) => GetNone
  case (GetNone, GetMany) => GetNone
  case (GetMany, GetNone) => GetNone
  case (GetNone, GetOption) => GetNone
  case (GetOption, GetNone) => GetNone
  case (GetNone, GetOne) => GetNone
  case (GetOne, GetNone) => GetNone
  case (GetMany, GetMany) => GetMany
  case (GetOption, GetMany) => GetMany
  case (GetMany, GetOption) => GetMany
  case (GetOption, GetOption) => GetOption
  case (GetOption, GetOne) => GetOption
  case (GetOne, GetOption) => GetOption
  case (GetOne, GetOne) => GetOne
  // case (ReplaceNone, ReplaceNone) => ReplaceNone
  // case (ReplaceNone, Replace) => ReplaceNone
  // case (Replace, ReplaceNone) => ReplaceNone
  // case (ReplaceNone, ReplaceAll) => ReplaceNone
  // case (ReplaceAll, ReplaceNone) => ReplaceNone
  // case (Replace, Replace) => Replace
  // case (Replace, ReplaceAll) => Replace
  // case (ReplaceAll, Replace) => Replace
  // case (ReplaceAll, ReplaceAll) => ReplaceAll


type GetterImplFor[C <: OpticCanGet, -S, +A] <: GetImpl[S,A] = C match
  case GetNone => GetNoneImpl[S,A]
  case GetMany => GetManyImpl[S,A]
  case GetOption => GetOptionImpl[S,A]
  case GetOne => GetOneImpl[S,A]

trait Optic[Can <: OpticCanGet, S, T, A, B]: 
  self => 

  protected val getterImpl: GetterImplFor[Can, S, A]
  //protected def setterImpl: OpticImpl.Replace[S,T,B]

  def andThen[Can2 <: OpticCanGet, C, D](o: Optic[Can2, A, B, C, D]): Optic[ComposedGet[Can, Can2], S, T, C, D] = 
    new Optic:
      override protected val getterImpl: GetterImplFor[ComposedGet[Can, Can2], S, C] = 
        self.getterImpl.andThen(o.getterImpl).asInstanceOf[GetterImplFor[ComposedGet[Can, Can2], S, C]]

      //override protected def setterImpl: OpticImpl.Replace[S,T,B] = ???
    
end Optic

object Optic: 
  def getOne[S,A](f: S => A): Optic[GetOne, S, S, A, A] = 
    new Optic: 
      override protected val getterImpl: GetOneImpl[S, A] = 
        new GetOneImpl:
          val get = f




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