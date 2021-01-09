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

  def impl[From: Type, To: Type](getterExpr: Expr[From => To])(using quotes: Quotes): Expr[Lens[From, To]] = {
    import quotes.reflect._
  

    ///////////// DOMAIN //////////////////
    case class ParseParams(argName: String, argType: TypeRepr, lambdaBody: Term)

    enum FocusAction {
      case Field(name: String, from: TypeRepr, to: TypeRepr)
      //case Attempt(from: TypeRepr, to: TypeRepr)
      //case Index(i: Term, indexType: TypeRepr, from: TypeRepr, to: TypeRepr)

      def from: TypeRepr
      def to: TypeRepr

      override def toString(): String = this match {
        case Field(name, from, to) => s"Field($name, ${from.show}, ${to.show})"
      }
    }

    enum FocusError {
      case NotACaseClass(className: String)
      case NotAConcreteClass(className: String)
      case DidNotDirectlyAccessArgument
      case NotASimpleLambdaFunction
      case UnexpectedCodeStructure(code: String)
      case CouldntFindFieldType(fromType: String, fieldName: String)
      case ComposeMismatch(type1: String, type2: String)

      def asResult: FocusResult = Left(this)
    }

    type FocusResult = Either[FocusError, List[FocusAction]]


    ///////////// PARSING //////////////////
    def unwrap(term: Term): Term = {
      term match {
        case Block(List(), inner) => unwrap(inner)
        case Inlined(_, _, inner) => unwrap(inner)
        case x => x
      }
    }

    object CaseClass {
      def unapply(term: Term): Option[Term] =
        term.tpe.classSymbol.flatMap { sym => 
          Option.when(sym.flags.is(Flags.Case))(term)
        }
    }

    //  val ValDef(field, typeTree, _) = t.classSymbol.get.memberField(x).tree
    def getFieldType(fromType: TypeRepr, fieldName: String): Option[TypeRepr] =
      fromType.classSymbol.flatMap { 
        _.memberField(fieldName) match {
          case sym if sym.isNoSymbol => None
          case sym => sym.tree match {
            case ValDef(_, typeTree, _) => Some(typeTree.tpe)
            case _ => None
          }
        }
      }

    object ExpectedLambdaFunction {
      def unapply(term: Term): Option[ParseParams] = 
        unwrap(term) match {
          case Lambda(List(ValDef(argName, typeTree, _)), body) => Some(ParseParams(argName, typeTree.tpe, body))
          case _ => None
        }
    }

    def parseLambdaBody(params: ParseParams): FocusResult = {
      def loop(remainingBody: Term, listSoFar: List[FocusAction]): FocusResult = {

        def addFieldAction(fromType: TypeRepr, fieldName: String): FocusResult = {
          getFieldType(fromType, fieldName) match {
            case Some(toType) => Right(FocusAction.Field(fieldName, fromType, toType) :: listSoFar)
            case None => FocusError.CouldntFindFieldType(fromType.show, fieldName).asResult
          }
        }

        remainingBody match {
          // Leftmost select on the lambda argument
          case Select(Ident(idName), fieldName) if idName == params.argName => addFieldAction(params.argType, fieldName)
          case Select(Ident(idName), fieldName) => FocusError.DidNotDirectlyAccessArgument.asResult

          // ? symbol to navigate the happy path of an Attempt
          //case Select(prefix, "?") => loop(prefix, FocusAction.Attempt(prefix.tpe, ) :: listSoFar)

          // Subsequent selects on field names
          case Select(CaseClass(prefix), fieldName) => addFieldAction(prefix.tpe.widen, fieldName).flatMap(loop(prefix, _))
          case Select(prefix, _) => FocusError.NotACaseClass(prefix.tpe.toString).asResult

          case unexpected => FocusError.UnexpectedCodeStructure(unexpected.toString).asResult
        }
      }
      loop(params.lambdaBody, Nil)
    }

    ///////////// CODE GENERATION //////////////////
    def generateGetter(field: String, from: Term): Term = 
      Select.unique(from, field) // o.field

    def generateSetter(field: String, from: Term, to: Term): Term = 
      Select.overloaded(from, "copy", Nil, NamedArg(field, to) :: Nil) // o.copy(field = value)

    def generateLens(field: String, fromType: TypeRepr, toType: TypeRepr): Term = {
      (fromType.asType, toType.asType) match {
        case ('[f], '[t]) => 
         '{
            val setter = (to: t) => (from: f) => ${
              generateSetter(field, '{from}.asTerm, '{to}.asTerm).asExprOf[f]
            }
            
            val getter = (from: f) => ${ 
              generateGetter(field, '{from}.asTerm).asExprOf[t] 
            }
            Lens.apply(getter, setter)
          }.asTerm
      }
    }

    def generateActionCode(action: FocusAction): Term = 
      action match {
        case FocusAction.Field(name, from, to) => generateLens(name, from, to)
        //case FocusAction.Attempt => '{ ??? }.asTerm
        //case FocusAction.Index(idx) => '{ ??? }.asTerm
      }

    def composeLensTerms(lens1: Term, lens2: Term): Term = {
      (lens1.tpe.asType, lens2.tpe.asType) match {
        case ('[EOptional[err1, from1, to1]], '[EOptional[err2, from2, to2]]) => 
          '{ 
            ${lens1.asExprOf[EOptional[err1, from1, to1]]} >>> ${lens2.asExprOf[EOptional[err2, to1, to2]]}
          }

        case ('[a], '[b]) =>
          report.error(s"unable to compose ${Type.show[a]} >>> ${Type.show[b]}")
          '{???}
      }
    }.asTerm

    def generateCode(actions: List[FocusAction]): Term = {
      actions.foldLeft('{Iso.id[From]}.asTerm) { (termSoFar, action) => 
        composeLensTerms(termSoFar, generateActionCode(action))
      }
    }

    ///////////// EXECUTION //////////////////
    def errorMessage(error: FocusError): String = error match {
      case FocusError.NotACaseClass(fromClass) => s"Expecting a case class in the 'From' position; found $fromClass"
      case FocusError.NotAConcreteClass(fromClass) => s"Expecting a concrete case class in the 'From' position; cannot reify type $fromClass"
      case FocusError.NotASimpleLambdaFunction => s"Expecting a lambda function that directly accesses a field. Example: `GenLens[Address](_.streetNumber)`"
      case FocusError.DidNotDirectlyAccessArgument => s"Expecting a lambda function that directly accesses a field. Example: `GenLens[Address](_.streetNumber)`"
      case FocusError.ComposeMismatch(type1, type2) => s"Could not compose $type1 >>> $type2"
      case FocusError.UnexpectedCodeStructure(code) => s"Unexpected code structure: $code"
      case FocusError.CouldntFindFieldType(fromType, fieldName) => s"Couldn't find type for $fromType.$fieldName"
    }

    val fromTypeIsConcrete = TypeRepr.of[From].classSymbol.isDefined
    val result: FocusResult = 
      getterExpr.asTerm match {
        case ExpectedLambdaFunction(params) if fromTypeIsConcrete => parseLambdaBody(params)
        case ExpectedLambdaFunction(_) => FocusError.NotASimpleLambdaFunction.asResult
        case _ => FocusError.NotAConcreteClass(Type.show[Type[From]]).asResult
      }

    result match {
      case Right(list) => generateCode(list).asExprOf[Lens[From,To]]
      case Left(error) => report.error(errorMessage(error)); '{???}
    }
  }


    /*
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
  }*/

  def apply[S] = new MkFocus[S]

  class MkFocus[S] {
    inline def apply[T](inline get: (S => T)): Lens[S,T] = 
      ${ Focus.impl('get) }
  }
}
