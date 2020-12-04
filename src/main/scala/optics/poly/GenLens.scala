package optics.poly

import functions.{Attempt, Index}
import scala.quoted.{Quotes, Expr, Type, quotes}
import scala.quoted.report.error
import Function.const



object dsl {
  extension [A, Err, B] (a: A) 
    def ?(using Attempt[A] { type To = B; type Error = Err }): B = ???

  extension [A, K, Err, B] (a: A)
    def idx(k: K)(using Index[A, K] { type To = B; type Error = Err }): B = ???
}

object GenLens {

  def impl[S: Type, T: Type](getter: Expr[S => T])(using Quotes): Expr[Lens[S, T]] = {
    import quotes.reflect._

    object Function {
      def unapply(t: Term): Option[(List[ValDef], Term)] = t match {
        case Lambda(params, body) => Some((params, body))
        case Inlined(_, Nil, body) => unapply(body)
        case _ => None
      }
    }

    def fold[A](term: Term)(a: A)(f: (Term, A) => A): A =
      term match {
        case term @ Select(Ident(_), _) =>
          f(term, a)
        case outer @ Select(inner, name) =>
          f(outer, fold(inner)(a)(f))
        case Block(List(), Inlined(_, _, term)) =>
          fold(term)(a)(f)
        case _ =>
          error(s"Unrecognized syntax in expression body.")
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
            error(s"Unsupported syntax. Please make sure the field `${name}` is a case class")
            (symbol, build)
          }
        case (_, (cls, term)) =>
          (cls, term)
      }._2.copy(value)
    }
    
    Term.of(getter) match {
      case Function(param :: Nil, body) =>
        TypeRepr.of[S].classSymbol.map { cls =>
          '{
            val setter = (t: T) => (s: S) => ${
              tree(cls, body)(Term.of('s), Term.of('t)).asExprOf[S]
            }
            Lens.apply($getter, setter)
          }
        }.getOrElse {
          error("Unsupported syntax. Please explicitly use a concrete class.")
          '{???}
        }
      case err =>
        error(s"Unsupported syntax. Example: `GenLens[Address](_.streetNumber)`, ${err.show}")
        '{???}
    }
  }

  def apply[S] = new MkGenLens[S]

  inline def uncurried[S, T](inline get: (S => T)): Lens[S, T] = ${ GenLens.impl('get )}

  class MkGenLens[S] {
    inline def apply[T](inline get: (S => T)): Lens[S, T] = ${ GenLens.impl('get) }
  }

}

object Focus {

  def impl[S: Type, T: Type](getter: Expr[S => T])(using q: Quotes) = {

    import q.reflect._
  
    object Function {
      def unapply(t: Term): Option[(Term)] = t match {
        case Inlined(None, Nil, lambda @ Lambda(params, body)) => Some((lambda))
        case _ => None
      }
    }

    object Path {

      private def recur(tree: Term, selects: List[String]): Option[(List[String], Term)] = tree match {
        case Select(qual, name) => recur(qual, name :: selects)
        // stop extraction at this stage
        case expr if selects.nonEmpty => Some((selects, expr))
        case _ => None
      }

      def unapply(t: Term): Option[(List[String], Term)] = recur(t, Nil)
    }

    object Operator0 {
      def unapply(t: Term): Option[(Ident, List[TypeTree], Term, Term)] = t match {
        case Apply(Apply(TypeApply(ident @ Ident(_), tpe), rhs :: Nil), witness :: Nil) =>
          Some((ident, tpe, witness, rhs))
        case _ => None
      }
    }

    object Operator1 {
      def unapply(t: Term): Option[(Ident, List[TypeTree], Term, Term, Term)] = t match {
        case Apply(Apply(Apply(TypeApply(ident @ Ident(_), tpe), rhs :: Nil), arg :: Nil), witness :: Nil) =>
          Some((ident, tpe, witness, arg, rhs))
        case _ => None
      }
    }

    case class State(from: List[TypeRepr], to: List[TypeRepr])

    def genLens(from: TypeRepr, to: TypeRepr, term: Term): Term = {
      (from.asType, to.asType) match {
        case ('[f], '[t]) => 
          Term.of('{ GenLens.uncurried[f, t](${term.asExprOf[f => t]}) })
      }
    }

    def selector(from: TypeRepr, to: TypeRepr, sel: List[String]): Term = {

      def body(term: Term, sel: List[String]): Term = {
        sel match {
          case Nil => term
          case x :: xs =>
            body(Select.unique(term, x), xs)
        }
      }

      (from.asType, to.asType) match {
        case ('[f], '[t]) => Term.of('{ 
          (s : f) => ${ body(Term.of('s), sel).asExprOf[t] } 
        })
      }
    }

    def fieldType(sel: List[String], t: TypeRepr): TypeRepr = {
      sel match {
        case Nil => t
        case x :: xs =>
          val ValDef(field, typeTree, _) = t.classSymbol.get.field(x).tree
          fieldType(xs, typeTree.tpe)
      }
    }

    //This is a little nasty, but sure beats having to compile by hand...
    def compose(x: Term, y: Term, a: TypeRepr, b: TypeRepr): Term = Term.of {
      (a.asType, b.asType) match {
        case ('[EOptional[err1, aa, bb]], '[EOptional[err2, cc, dd]]) =>
          '{ ${x.asExprOf[EOptional[err1, aa, bb]] } >>> ${ y.asExprOf[EOptional[err2, bb, dd]] } }
        case ('[aa], '[bb]) =>
          error(s"unable to compose ${Type.show[aa]} >>> ${Type.show[bb]}")
          '{???}
      }
    }


    def fold(term: Term)(state: State): (State, Term) = {
      term match {
        case Function(Lambda(_, body)) =>
          fold(body)(state)
        case ident @ Ident(_) =>
          state match {
            case State(from :: Nil, _) =>
              from.asType match {
                case '[a] =>
                    (state, Term.of('{ Iso.id[a] }))
              }
          }
        case Path(sel, Ident(_)) =>
          state match {
            case State(from :: Nil, _) =>
              val to = fieldType(sel, from)
              (
                state.copy(from = to :: state.from),
                genLens(from, to, selector(from, to, sel))
              )
            case _ => ???
          }
        case Operator1(Ident("idx"), _ :: key :: err :: to :: Nil, witness, arg, rhs) =>
          fold(rhs)(state) match {
            case (State(from :: _, _), term) =>
              (err.tpe.asType, from.asType, to.tpe.asType) match {
                case ('[e], '[a], '[b]) =>
                  val lens = compose(
                    term,
                    Apply(
                      Select.unique(witness, "index"),
                      List(arg)
                    ), 
                    term.tpe, 
                    TypeRepr.of[EOptional[e, a, b]])
                  (
                    state.copy(from = to.tpe :: state.from),
                    lens
                  )
              }

          }
        case Operator0(Ident("?"), _ :: err :: to :: Nil, witness, rhs) =>
          fold(rhs)(state) match {
            case (State(from :: _, _), term) =>
              (err.tpe.asType, from.asType, to.tpe.asType) match {
                case ('[e], '[a], '[b]) =>

                  val lens = compose(
                    term,
                    Select.unique(witness, "attempt"), 
                    term.tpe, 
                    TypeRepr.of[EOptional[e, a, b]])
                  (
                    state.copy(from = to.tpe :: state.from),
                    lens
                  )
              }
          }
        case Path(sel, term) =>
          fold(term)(state) match {
            case (State(from :: _, _), term) =>
              val to = fieldType(sel, from)
              val lens = genLens(
                from,
                to,
                selector(from, to, sel)
              )
              (
                state.copy(from = to :: state.from),
                compose(
                  term,
                  lens, 
                  term.tpe, 
                  lens.tpe)
              )
          }
      }
    }

    val (_, term) = fold(Term.of(getter))(
      State(List(TypeRepr.of[S]), 
      List(TypeRepr.of[T]))
    )
    term.asExprOf[Lens[S,T]]
  }

  def apply[S] = new MkFocus[S]

  class MkFocus[S] {
    inline def apply[T](inline get: (S => T)): Lens[S,T] = ${ Focus.impl('get) }
  }
}
