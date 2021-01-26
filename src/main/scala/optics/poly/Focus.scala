package optics.poly

import functions.Index
import scala.quoted.{Quotes, Expr, Type, quotes}
import Function.const


object dsl {
  extension [A, B] (a: Option[A])
    def ? : B = ???

  extension [A, K, B] (a: A)
    def idx(k: K)(using Index[A, K] { type To = B}): B = ???
}

object Focus {

  def impl[From: Type, To: Type](getterExpr: Expr[From => To])(using Quotes): Expr[Lens[From, To]] = {
    import quotes.reflect._

    ///////////// DOMAIN //////////////////
    case class ParseParams(argName: String, argType: TypeRepr, lambdaBody: Term)

    // Common type information that we record about every action in the DSL
    case class TypeInfo(from: TypeRepr, fromTypeArgs: List[TypeRepr], to: TypeRepr) {
      override def toString(): String = 
        s"TypeInfo(${from.show}, ${fromTypeArgs.map(_.show).mkString("[", ",", "]")}, ${to.show})"
    }

    enum FocusAction {
      case Field(name: String, typeInfo: TypeInfo)
      //case Attempt(info: TypeInfo)
      //case Index(i: Term, indexType: TypeRepr, info: TypeInfo)

      def typeInfo: TypeInfo

      override def toString(): String = this match {
        case Field(name, info) => s"Field($name, $info)"
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

      def asResult: FocusResult[Nothing] = Left(this)
    }


    type FocusResult[+A] = Either[FocusError, A]
    type ParseResult = FocusResult[List[FocusAction]]

    ///////////// PARSING //////////////////
    def unwrap(term: Term): Term = {
      term match {
        case Block(List(), inner) => unwrap(inner)
        case Inlined(_, _, inner) => unwrap(inner)
        case x => x
      }
    }

    object ExpectedLambdaFunction {
      def unapply(term: Term): Option[ParseParams] = 
        unwrap(term) match {
          case Lambda(List(ValDef(argName, typeTree, _)), body) => Some(ParseParams(argName, typeTree.tpe, body))
          case _ => None
        }
    }

    object CaseClass {
      def unapply(term: Term): Option[Term] =
        term.tpe.classSymbol.flatMap { sym => 
          Option.when(sym.flags.is(Flags.Case))(term)
        }
    }

    object FieldType {
      def unapply(fieldSymbol: Symbol): Option[TypeRepr] = fieldSymbol match {
        case sym if sym.isNoSymbol => None
        case sym => sym.tree match {
          case ValDef(_, typeTree, _) => Some(typeTree.tpe)
          case _ => None
        }
      }
    }

    def getDeclaredTypeArgs(classType: TypeRepr): List[Symbol] = {
      classType.classSymbol.map(_.primaryConstructor.paramSymss) match {
        case Some(typeParamList :: _) if typeParamList.exists(_.isTypeParam) => typeParamList
        case _ => Nil
      }
    }
    
    def getSuppliedTypeArgs(fromType: TypeRepr): List[TypeRepr] = {
      fromType match {
        case AppliedType(_, argTypeReprs) => argTypeReprs 
        case _ => Nil
      }
    }

    def swapWithSuppliedType(fromType: TypeRepr, possiblyContainsTypeArgs: TypeRepr): TypeRepr = {
      val declared = getDeclaredTypeArgs(fromType)
      val supplied = getSuppliedTypeArgs(fromType)
      val swapDict = declared.view.map(_.name).zip(supplied).toMap
      
      def swapInto(candidate: TypeRepr): TypeRepr = {
        candidate match {
          // Waiting until we can get an AppliedType constructor
          //case AppliedType(typeCons, args) => AppliedType(swapInto(typeCons), args.map(swapInto))
          case leafType => swapDict.getOrElse(leafType.typeSymbol.name, leafType)
        }
      }
      swapInto(possiblyContainsTypeArgs)
    }

    def getFieldType(fromType: TypeRepr, fieldName: String): Option[TypeRepr] = {
      fromType.classSymbol.flatMap { 
        _.memberField(fieldName) match {
          case FieldType(possiblyTypeArg) => Some(swapWithSuppliedType(fromType, possiblyTypeArg))
          case _ => None
        }
      }
    }

    def parseLambdaBody(params: ParseParams): ParseResult = {
      def loop(remainingBody: Term, listSoFar: List[FocusAction]): ParseResult = {

        def addFieldAction(fromType: TypeRepr, fieldName: String): ParseResult = {
          getFieldType(fromType, fieldName) match {
            case Some(toType) => Right(FocusAction.Field(fieldName, TypeInfo(fromType, getSuppliedTypeArgs(fromType), toType)) :: listSoFar)
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

          case unexpected => FocusError.UnexpectedCodeStructure(unexpected.show).asResult
        }
      }
      loop(params.lambdaBody, Nil)
    }

    ///////////// CODE GENERATION //////////////////
    def generateGetter(field: String, from: Term): Term = 
      Select.unique(from, field) // o.field

    def generateSetter(field: String, from: Term, to: Term, typeInfo: TypeInfo): Term = {
      Select.overloaded(from, "copy", typeInfo.fromTypeArgs, NamedArg(field, to) :: Nil) // o.copy(field = value)
    }

    def generateLens(field: String, typeInfo: TypeInfo): Term = {
      (typeInfo.from.asType, typeInfo.to.asType) match {
        case ('[f], '[t]) => 
         '{
            val setter = (to: t) => (from: f) => ${
              generateSetter(field, '{from}.asTerm, '{to}.asTerm, typeInfo).asExprOf[f]
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
        case FocusAction.Field(name, typeInfo) => generateLens(name, typeInfo)
        //case FocusAction.Attempt => '{ ??? }.asTerm
        //case FocusAction.Index(idx) => '{ ??? }.asTerm
      }

    def composeLensTerms(lens1: Term, lens2: Term): FocusResult[Term] = {
      (lens1.tpe.asType, lens2.tpe.asType) match {
        case ('[Optional[from1, to1]], '[Optional[from2, to2]]) =>
          Right('{ 
            ${lens1.asExprOf[Optional[from1, to1]]}.andThen(${lens2.asExprOf[Optional[to1, to2]]})
          }.asTerm)
        case ('[a], '[b]) => FocusError.ComposeMismatch(TypeRepr.of[a].show, TypeRepr.of[b].show).asResult
      }
    }

    def generateCode(actions: List[FocusAction]): FocusResult[Term] = {
      val idLens: FocusResult[Term] = Right('{Iso.id[From]}.asTerm)
      
      actions.foldLeft(idLens) { (resultSoFar, action) => 
        resultSoFar.flatMap(term => composeLensTerms(term, generateActionCode(action)))
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

    val parseResult: ParseResult = 
      getterExpr.asTerm match {
        case ExpectedLambdaFunction(params) if fromTypeIsConcrete => parseLambdaBody(params)
        case ExpectedLambdaFunction(_) => FocusError.NotASimpleLambdaFunction.asResult
        case _ => FocusError.NotAConcreteClass(Type.show[Type[From]]).asResult
      }

    val generatedCode: FocusResult[Term] = 
      parseResult.flatMap(generateCode)
      
    generatedCode match {
      case Right(code) => code.asExprOf[Lens[From,To]]
      case Left(error) => report.error(errorMessage(error)); '{???}
    }
  }
 
  def apply[S] = new MkFocus[S]

  class MkFocus[S] {
    inline def apply[T](inline get: (S => T)): Lens[S,T] = 
      ${ Focus.impl('get) }
  }
}
