package optics.internal.focus
import scala.quoted.Quotes

trait MacroContext {
  val macroContext: Quotes 

  given Quotes = macroContext

  type Term = macroContext.reflect.Term
  type TypeRepr = macroContext.reflect.TypeRepr
}