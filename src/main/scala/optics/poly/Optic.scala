package optics.poly

type Traversal[From, To] = PTraversal[From, From, To, To]
type Optional[From, To]  = POptional[From, From, To, To]
type Prism[From, To]     = PPrism[From, From, To, To]
type Lens[From, To]      = PLens[From, From, To, To]
type Iso[From, To]       = PIso[From, From, To, To]



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

  protected[optics] val getterImpl: GetterImplFor[Can, S, A]
  //protected def setterImpl: OpticImpl.Replace[S,T,B]

  def andThen[Can2 <: OpticCanGet, C, D](o: Optic[Can2, A, B, C, D]): Optic[ComposedGet[Can, Can2], S, T, C, D] = 
    new Optic:
      override protected[optics] val getterImpl: GetterImplFor[ComposedGet[Can, Can2], S, C] = 
        self.getterImpl.andThen(o.getterImpl).asInstanceOf[GetterImplFor[ComposedGet[Can, Can2], S, C]]

      //override protected def setterImpl: OpticImpl.Replace[S,T,B] = ???
    
end Optic

extension [S, T, A, B] (optic: Optic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getterImpl.get

extension [S, T, A, B] (optic: Optic[GetOption, S, T, A, B])
  inline def getOption: S => Option[A] = optic.getterImpl.getOption

object Optic: 
  def buildGetOne[S,A](f: S => A): Optic[GetOne, S, S, A, A] = 
    new Optic: 
      override protected[optics] val getterImpl: GetOneImpl[S, A] = 
        new GetOneImpl:
          val get = f
  end buildGetOne

  def buildGetOption[S,A](f: S => Option[A]): Optic[GetOption, S, S, A, A] = 
    new Optic: 
      override protected[optics] val getterImpl: GetOptionImpl[S, A] = 
        new GetOptionImpl:
          val getOption = f
  end buildGetOption
end Optic
