package optics.internal.focus

import optics.poly.Lens
import optics.internal.focus.parse.AllParsers
import optics.internal.focus.codegen.AllGenerators
import scala.quoted.{Type, Expr, Quotes, quotes}

private[focus] class FocusImpl(val macroContext: Quotes) 
    extends MacroContext 
    with DomainModule 
    with ErrorHandlingModule 
    with AllParsers 
    with AllGenerators {

  given Quotes = macroContext
  import macroContext.reflect._

  def run[From: Type, To: Type](lambda: Expr[From => To]): Expr[Lens[From, To]] = {
    val parseResult: FocusResult[List[FocusAction]] = 
      parseLambda(Term.of(lambda))

    val generatedCode: FocusResult[Term] = 
      parseResult.flatMap(generateCode)
      
    generatedCode match {
      case Right(code) => code.asExprOf[Lens[From,To]]
      case Left(error) => report.error(errorMessage(error)); '{???}
    }
  }
}

object FocusImpl {
  def apply[From: Type, To: Type](lambda: Expr[From => To])(using Quotes): Expr[Lens[From, To]] =
    new FocusImpl(quotes).run(lambda)
}