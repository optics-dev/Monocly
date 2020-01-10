package optics

import scala.annotation.alpha
import scala.quoted._

trait PLens[-S, +T, +A, -B] extends EPOptional[Nothing, S, T, A, B] { self =>
  def get(from: S): A

  def getOrModify(from: S): Either[(Nothing, T), A] = Right(get(from))

  override def modify(f: A => B): S => T = from => set(f(get(from)))(from)

  @alpha("andThen")
  def >>>[C, D](other: PLens[A, B, C, D]): PLens[S, T, C, D] = new PLens[S, T, C, D] {
    def get(from: S): C = other.get(self.get(from))
    def set(to: D): S => T = self.modify(other.set(to))
    override def modify(f: C => D): S => T = self.modify(other.modify(f))
  }
}

object PLens {
  def apply[S, T, A, B](_get: S => A)(_set: B => S => T): PLens[S, T, A, B] = new PLens[S, T, A, B] {
    def get(from: S): A = _get(from)
    def set(to: B): S => T = _set(to)
  }
}

object Lens {
  def apply[A, B](_get: A => B)(_set: B => A => A): Lens[A, B] = PLens(_get)(_set)

  def impl[A: Type, B: Type](getter: Expr[A => B])(given qctx: QuoteContext): Expr[Lens[A, B]] = {
    implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(this.getClass.getClassLoader)
    import qctx.tasty.{_, given}
    import util._
    // obj.copy(field = value)
    def setterBody(obj: Expr[A], value: Expr[B], field: String): Expr[A] =
      Select.overloaded(obj.unseal, "copy", Nil, NamedArg(field, value.unseal) :: Nil).seal.cast[A]

    // exception: getter.unseal.underlyingArgument
    getter.unseal match {
      case Inlined(
        None, Nil,
        Block(
          DefDef(_, Nil, (param :: Nil) :: Nil, _, Some(Select(o, field))) :: Nil,
          Lambda(meth, _)
        )
      ) if o.symbol == param.symbol =>
        '{
         val setter = (to: B) => (from: A) => ${ setterBody('s, 't, field) }
         apply($getter)(setter)
        }
      case _ =>
        qctx.error("Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`")
        '{???}
    }
  }
}

object GenLens {
  def apply[A] = new MkGenLens[A]
  class MkGenLens[A] {
    inline def apply[B](get: => (A => B)): Lens[A, B] = ${ Lens.impl('get) }
  }
}