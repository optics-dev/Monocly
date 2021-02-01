package optics.internal.focus

import optics.poly.Lens
import scala.quoted.{Type, Expr, Quotes, quotes}

private[focus] class FocusImpl(val macroContext: Quotes) 
    extends FocusBase 
    with ErrorHandling
    with ParserLoop with AllParsers 
    with GeneratorLoop with AllGenerators {

  import macroContext.reflect._

  def run[From: Type, To: Type](lambda: Expr[From => To]): Expr[Lens[From, To]] = {
    val parseResult: FocusResult[List[FocusAction]] = 
      parseLambda[From](lambda.asTerm)

    val generatedCode: FocusResult[Term] = 
      parseResult.flatMap(generateCode[From])
      
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