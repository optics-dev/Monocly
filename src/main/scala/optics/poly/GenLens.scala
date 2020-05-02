package optics.poly

import scala.quoted._

object GenLens {

  def impl[S: Type, T: Type](getter: Expr[S => T])(using qctx: QuoteContext): Expr[Lens[S, T]] = {

    import qctx.tasty._
    import util._

    object Function {
      def unapply(t: Term): Option[(List[ValDef], Term)] = t match {
        case Inlined(None, Nil, Lambda(params, body)) => Some((params, body))
        case _ => None
      }
    }

    def fold[A](term: Term)(a: A)(f: (Term, A) => A): A =
      term match {
        case term @ Select(Ident(_), _) =>
          f(term, a)
        case outer @ Select(inner, name) =>
          f(outer, fold(inner)(a)(f))
        case Apply(Apply(TypeApply(sym @ Ident("?"), _), rhs :: Nil), _) => {
          fold(rhs)(a)(f)
        }
        case tree =>
          qctx.error(s"Unrecognized syntax. $tree")
          a
      }

    case class State(
      copy: Term => Term, //obj.copy(bar = _)
      sel: Term //obj.c
    )

    def tree(cls: Symbol, expr: Term)(obj: Term, value: Term): Term = {

      // o.copy(field = value)
      def setter(obj: Term, field: String): Term => Term =
        value => Select.overloaded(obj, "copy", Nil, NamedArg(field, value) :: Nil)

      fold(expr)((cls, State(identity[Term], obj))) {
        case (Select(Ident(_), field), (cls, State(copy, sel))) if cls.flags.is(Flags.Case) =>
          (cls.field(field), State(setter(sel, field), Select.unique(obj, field)))
        case (Select(_, name), (symbol, build @ State(copy, sel))) if symbol.isValDef =>
          val ValDef(field, typeTree, _) = symbol.tree
          typeTree.tpe.classSymbol.filter(_.flags.is(Flags.Case)).map { cls =>
            (
              cls.field(name),
              State(
                value => copy(setter(sel, name)(value)),
                Select.unique(sel, name)
              )
            )
          }.getOrElse {
            qctx.error(s"Unsupported syntax. Please make sure the field `${name}` is a case class")
            (symbol, build)
          }
        case (_, (cls, term)) =>
          (cls, term)
      }._2.copy(value)
    }

    getter.unseal match {
      case Function(param :: Nil, body) =>
        typeOf[S].classSymbol.map { cls =>
          '{
            val setter = (t: T) => (s: S) => ${
              tree(cls, body)(('s).unseal, ('t).unseal).seal.cast[S]
            }
            Lens.apply($getter, setter)
          }
        }.getOrElse {
          qctx.error("Unsupported syntax. Please explicitly use a concrete class.")
          '{???}
        }
      case err =>
        qctx.error(s"Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`, $err")
        '{???}
    }
  }

  def apply[S] = new GenLens[S]

  class GenLens[S] {
    inline def apply[T](inline get: (S => T)): Lens[S, T] = ${ GenLens.impl('get) }
  }

}
