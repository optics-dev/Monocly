package optics.poly

sealed trait OpticCan
class GetMany extends OpticCan
class GetOption extends GetMany
class GetOne extends GetOption
class Replace extends OpticCan
class ReplaceAll extends Replace


final class Optic[+ThisCan <: OpticCan, S, T, A, B] protected[optics](
  protected[optics] val getterImpl: GetImpl[ThisCan, S, A],
  protected[optics] val setterImpl: SetImpl[ThisCan, S, T, A, B]):

  def andThen[ThatCan <: OpticCan, AllowedByBoth >: (ThisCan | ThatCan) <: OpticCan, C, D](o: Optic[ThatCan, A, B, C, D]): Optic[AllowedByBoth, S, T, C, D] = 
    Optic(getterImpl.andThen(o.getterImpl), setterImpl.andThen(o.setterImpl))

end Optic

extension [S, T, A, B] (optic: Optic[GetOne, S, T, A, B])
  inline def get: S => A = optic.getterImpl.doGet

extension [S, T, A, B] (optic: Optic[GetOption, S, T, A, B])
 inline def getOption: S => Option[A] = optic.getterImpl.doGetOption

object Optic: 
  def withGetOne[S,A](f: S => A): Optic[GetOne, S, S, A, A] = 
    Optic(GetOneImpl(f), ReplaceNoneImpl)

  def withGetOption[S,A](f: S => Option[A]): Optic[GetOption, S, S, A, A] = 
    Optic(GetOptionImpl(f), ReplaceNoneImpl)

  def withGetMany[S,A](f: S => List[A]): Optic[GetMany, S, S, A, A] = 
    Optic(GetManyImpl(f), ReplaceNoneImpl)

end Optic
