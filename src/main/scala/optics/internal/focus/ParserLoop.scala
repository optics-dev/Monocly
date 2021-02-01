package optics.internal.focus

import scala.quoted.Type
import optics.internal.focus.features.fieldselect.FieldSelectParser

type AllParsers = FieldSelectParser // & ...

trait ParserLoop {
  this: FocusBase with AllParsers => 

  import macroContext.reflect._
  
  def parseLambda[From: Type](lambda: Term): ParseResult = {
    val fromTypeIsConcrete = TypeRepr.of[From].classSymbol.isDefined

    lambda match {
      case ExpectedLambdaFunction(params) if fromTypeIsConcrete => parseLambdaBody(params)
      case ExpectedLambdaFunction(_) => FocusError.NotASimpleLambdaFunction.asResult
      case _ => FocusError.NotAConcreteClass(Type.show[Type[From]]).asResult
    }
  }

  case class ParseParams(argName: String, argType: TypeRepr, lambdaBody: Term)

  private def parseLambdaBody(params: ParseParams): ParseResult = {
    def loop(remainingBody: Term, listSoFar: List[FocusAction]): ParseResult = {

      def addFieldAction(fromType: TypeRepr, fieldName: String): ParseResult = {
        getFieldType(fromType, fieldName) match {
          case Some(toType) => Right(FocusAction.Field(fieldName, TypeInfo(fromType, getSuppliedTypeArgs(fromType), toType)) :: listSoFar)
          case None => FocusError.CouldntFindFieldType(fromType.show, fieldName).asResult
        }
      }

      remainingBody match {
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

  def unwrap(term: Term): Term = {
    term match {
      case Block(List(), inner) => unwrap(inner)
      case Inlined(_, _, inner) => unwrap(inner)
      case x => x
    }
  }

  private object ExpectedLambdaFunction {
    def unapply(term: Term): Option[ParseParams] = 
      unwrap(term) match {
        case Lambda(List(ValDef(argName, typeTree, _)), body) => Some(ParseParams(argName, typeTree.tpe, body))
        case _ => None
      }
  }
}