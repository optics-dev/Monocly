package optics.internal.focus
import scala.quoted.Quotes

trait FocusBase {
  val macroContext: Quotes 

  given Quotes = macroContext

  type Term = macroContext.reflect.Term
  type TypeRepr = macroContext.reflect.TypeRepr

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
}