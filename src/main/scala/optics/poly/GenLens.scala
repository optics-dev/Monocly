package optics.poly

import functions.{Attempt, Index}
import scala.quoted.{Quotes, Expr, Type, quotes}
import Function.const



object dsl {
  extension [A, Err, B] (a: A) 
    def ?(using Attempt[A] { type To = B; type Error = Err }): B = ???

  extension [A, K, Err, B] (a: A)
    def idx(k: K)(using Index[A, K] { type To = B; type Error = Err }): B = ???
}



object GenLens {

  def impl[From: Type, To: Type](getterExpr: Expr[From => To])(using quotes: Quotes): Expr[Lens[From, To]] = {
    import quotes.reflect._

    def unwrap(term: Term): Term = {
      term match {
        case Block(List(), inner) => unwrap(inner)
        case Inlined(_, _, inner) => unwrap(inner)
        case x => x
      }
    }

    object ExpectedLambdaFunction {
      def unapply(term: Term): Option[String] = unwrap(term) match {
        case Lambda(List(ValDef(argName, _, _)), Select(Ident(identifier), fieldName)) if argName == identifier => Some(fieldName)
        case _ => None
      }
    }

    def constructGetterBody(field: String, from: Term): Term = {
      // o.copy(field = value)
      Select.unique(from, field)
    }

    def constructSetterBody(field: String, from: Term, to: Term): Term = {
      // o.copy(field = value)
      Select.overloaded(from, "copy", Nil, NamedArg(field, to) :: Nil)
    }

    def constructLensExpr(field: String): Expr[Lens[From, To]] = 
      '{
        val setter = (to: To) => (from: From) => ${
          constructSetterBody(field, '{from}.asTerm, '{to}.asTerm).asExprOf[From]
        }
        
        val getter = (from: From) => ${ 
          constructGetterBody(field, '{from}.asTerm).asExprOf[To] 
        }
        Lens.apply(getter, setter)
      }

    getterExpr.asTerm match {
      case ExpectedLambdaFunction(fieldName) =>
        TypeRepr.of[From].classSymbol match {
          case Some(fromClass) if fromClass.flags.is(Flags.Case) => constructLensExpr(fieldName)
          case Some(fromClass) => report.error(s"Expecting a case class in the 'From' position; found $fromClass"); '{???}
          case None => report.error(s"Expecting a concrete case class in the 'From' position; cannot reify type ${summon[Type[From]]}"); '{???}
        }
      case term =>
        report.error(s"Expecting a lambda function that directly accesses a field. Example: `GenLens[Address](_.streetNumber)`")
        '{???}
    }
  }

  def apply[From] = new MkGenLens[From]

  inline def uncurried[From, To](inline get: (From => To)): Lens[From, To] = 
    ${ GenLens.impl('get) }

  class MkGenLens[From] {
    inline def apply[To](inline get: (From => To)): Lens[From, To] = 
      ${ GenLens.impl('get) }
  }

}

object Focus {

  def impl[S: Type, T: Type](getter: Expr[S => T])(using quotes: Quotes): Expr[Lens[S, T]] = {
    import quotes.reflect._
  
    /*
    ///////////// PARSING //////////////////
    def unwrap(term: Term): Term = {
      term match {
        case Block(List(), inner) => unwrap(inner)
        case Inlined(_, _, inner) => unwrap(inner)
        case x => x
      }
    }

    object ExpectedLambdaFunction {
      def unapply(term: Term): Option[Term] = unwrap(term) match {
        case Lambda(List(ValDef(_, _, _)), body) => Some(body)
        case _ => None
      }
    }

    enum FocusAction {
      case Field(name: String, from: TypeRepr, to: TypeRepr)
      case Attempt
      case Index(i: Term)
    }

    enum FocusError {
      case NotACaseClass(className: String)
      case NotAConcreteClass(className: String)
      case NotASimpleLambdaFunction
      case LambdaBodyIsWeird
    }

    type FocusResult = FocusError | List[FocusAction]

    def parseLambdaBody(body: Term): FocusResult = {
      def loop(remainingBody: Term, listSoFar: List[FocusAction]): FocusResult = {
        remainingBody match {
          case Select(prefix, "?") => FocusAction.Attempt :: listSoFar
          case Select(ident @ Ident(_), fieldName) => 
            ident.
            FocusAction.Field(fieldName) :: listSoFar
          case Select(prefix, fieldName) => loop(prefix, FocusAction.Field(fieldName) :: listSoFar)
          case _ => FocusError.LambdaBodyIsWeird
        }
      }
      loop(body, Nil)
    }

    ///////////// GENERATING //////////////////
    def constructGetterBody(field: String, from: Term): Term = 
      Select.unique(from, field) // o.field

    def constructSetterBody(field: String, from: Term, to: Term): Term = 
      Select.overloaded(from, "copy", Nil, NamedArg(field, to) :: Nil) // o.copy(field = value)

    def constructLensExpr[From2: Type, To2: Type](field: String): Expr[Lens[From2, To2]] = 
      '{
        val setter = (to: To2) => (from: From2) => ${
          constructSetterBody(field, Term.of('{from}), Term.of('{to})).asExprOf[From2]
        }
        
        val getter = (from: From2) => ${ 
          constructGetterBody(field, Term.of('{from})).asExprOf[To2] 
        }
        Lens.apply(getter, setter)
      }

    case class TypedLens(lens: Term, from: TypeRepr, to: TypeRepr)

    def generateCode(actions: List[FocusAction]): Expr[Lens[From, To]] = {
      def generateActionCode(action: FocusAction): Expr[Lens[From, To]] = 
        action match {
          case FocusAction.Field(name: String) => constructLensExpr(name)
          case FocusAction.Attempt => '{ ??? }
          case FocusAction.Index(idx) => '{ ??? }
        }

      actions.foldLeft('{Lens.id[To]}) { (lensExpr, action) => 
        '{${lensExpr} >>> ${generateActionCode(action)}}
      }
    }
      
    ///////////// EXECUTION //////////////////
    val result: FocusResult = 
      Term.of(getterExpr) match {
        case ExpectedLambdaFunction(body) =>
          TypeRepr.of[From].classSymbol match {
            case Some(fromClass) if fromClass.flags.is(Flags.Case) => parseLambdaBody(body)
            case Some(fromClass) => FocusError.NotACaseClass(fromClass.fullName)
            case None => FocusError.NotAConcreteClass(Type.show[Type[From]])
          }
        case _ => FocusError.NotASimpleLambdaFunction
      }

    ///////////// ERROR HANDLING //////////////////
    result match {
      case List(actions: _*) => generateCode(action :: actions)
      case FocusError.NotACaseClass(fromClass) => error(s"Expecting a case class in the 'From' position; found $fromClass"); '{???}
      case FocusError.NotAConcreteClass(fromClass) => error(s"Expecting a concrete case class in the 'From' position; cannot reify type $fromClass"); '{???}
      case FocusError.NotASimpleLambdaFunction => error(s"Expecting a lambda function that directly accesses a field. Example: `GenLens[Address](_.streetNumber)`"); '{???}
      case FocusError.LambdaBodyIsWeird => error(s"Expecting a lambda function that directly accesses a field. Example: `GenLens[Address](_.streetNumber)`"); '{???}
    }
  }*/


    
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
          '{ GenLens.uncurried[f, t](${term.asExprOf[f => t]}) }.asTerm
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
        case ('[f], '[t]) => '{ 
          (sz : f) => ${ body('{sz}.asTerm, sel).asExprOf[t] }
        }.asTerm
      }
    }

    def fieldType(sel: List[String], t: TypeRepr): TypeRepr = {
      sel match {
        case Nil => t
        case x :: xs =>
          val ValDef(field, typeTree, _) = t.classSymbol.get.memberField(x).tree
          fieldType(xs, typeTree.tpe)
      }
    }

    //This is a little nasty, but sure beats having to compile by hand...
    def compose(x: Term, y: Term, a: TypeRepr, b: TypeRepr): Term = {
      (a.asType, b.asType) match {
        case ('[EOptional[err1, aa, bb]], '[EOptional[err2, cc, dd]]) =>
          '{ ${x.asExprOf[EOptional[err1, aa, bb]] } >>> ${ y.asExprOf[EOptional[err2, bb, dd]] } }
        case ('[aa], '[bb]) =>
          report.error(s"unable to compose ${Type.show[aa]} >>> ${Type.show[bb]}")
          '{???}
      }
    }.asTerm


    def fold(term: Term)(state: State): (State, Term) = {
      term match {
        case Function(Lambda(_, body)) =>
          fold(body)(state)
        case ident @ Ident(_) =>
          state match {
            case State(from :: Nil, _) =>
              from.asType match {
                case '[a] =>
                    (state, '{ Iso.id[a] }.asTerm)
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
              val lens = genLens(from, to, selector(from, to, sel))
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

    val (_, term) = fold(getter.asTerm)(
      State(List(TypeRepr.of[S]), 
      List(TypeRepr.of[T]))
    )
    term.asExprOf[Lens[S,T]]
  }

  def apply[S] = new MkFocus[S]

  class MkFocus[S] {
    inline def apply[T](inline get: (S => T)): Lens[S,T] = 
      ${ Focus.impl('get) }
  }
}
