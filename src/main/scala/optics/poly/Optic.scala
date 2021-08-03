package optics.poly

type Traversal[From, To] = PTraversal[From, From, To, To]
type Optional[From, To]  = POptional[From, From, To, To]
type Prism[From, To]     = PPrism[From, From, To, To]
type Lens[From, To]      = PLens[From, From, To, To]
type Iso[From, To]       = PIso[From, From, To, To]


sealed class OpticCanGet
class GetNone extends OpticCanGet
class GetMany extends GetNone
class GetOption extends GetMany
class GetOne extends GetOption

sealed trait OpticCanSet
class ReplaceNone extends OpticCanSet
class Replace extends ReplaceNone
class ReplaceAll extends Replace


abstract class Optic[+ThisCan <: OpticCanGet, S, T, A, B]: 
  self => 

  protected[optics] def getterImpl: GetImpl[ThisCan, S, A]
  //protected def setterImpl: OpticImpl.Replace[S,T,B]

  def andThen[ThatCan <: OpticCanGet, C, D](o: Optic[ThatCan, A, B, C, D]): Optic[ThisCan | ThatCan, S, T, C, D] = 
    new Optic:
      override protected[optics] val getterImpl: GetImpl[ThisCan | ThatCan, S, C] = 
        self.getterImpl.andThen(o.getterImpl)
    end new
    
end Optic

extension [S, T, A, B] (optic: Optic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getterImpl.doGet

extension [S, T, A, B] (optic: Optic[GetOption, S, T, A, B])
 inline def getOption: S => Option[A] = optic.getterImpl.doGetOption

object Optic: 

  def withGetOne[S,A](f: S => A): Optic[GetOne, S, S, A, A] = 
    new Optic: 
      override protected[optics] val getterImpl: GetOneImpl[S, A] = 
        new GetOneImpl:
          val get = f
  end withGetOne

  def withGetOption[S,A](f: S => Option[A]): Optic[GetOption, S, S, A, A] = 
    new Optic: 
      override protected[optics] val getterImpl: GetOptionImpl[S, A] = 
        new GetOptionImpl:
          val getOption = f
  end withGetOption
end Optic
