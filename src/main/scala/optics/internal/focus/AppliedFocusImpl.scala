package optics.internal.focus

import optics.poly.{Focus, Lens, Iso, Prism, Optional}
import scala.quoted.{Type, Expr, Quotes, quotes}


object AppliedFocusImpl {
  def apply[From: Type, To: Type](from: Expr[From], lambda: Expr[From => To])(using Quotes): Expr[Any] = {
    import quotes.reflect._

    val generatedOptic = new FocusImpl(quotes).run(lambda)

    generatedOptic.asTerm.tpe.asType match {
      case '[Lens[f, t]] => '{ _root_.optics.poly.Focus.AppliedLens[From, From, To, To]($from, ${generatedOptic.asExprOf[Lens[From,To]]}) }
      case '[Prism[f, t]] => '{ _root_.optics.poly.Focus.AppliedPrism[From, From, To, To]($from, ${generatedOptic.asExprOf[Prism[From,To]]}) }
      case '[Iso[f, t]] => '{ _root_.optics.poly.Focus.AppliedIso[From, From, To, To]($from, ${generatedOptic.asExprOf[Iso[From,To]]}) }
      case '[Optional[f, t]] => '{ _root_.optics.poly.Focus.AppliedOptional[From, From, To, To]($from, ${generatedOptic.asExprOf[Optional[From,To]]}) }
    }
  }
}