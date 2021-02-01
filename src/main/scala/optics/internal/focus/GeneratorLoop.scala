package optics.internal.focus

import optics.internal.focus.features.fieldselect.FieldSelectGenerator
import optics.poly.{Lens, Iso}
import scala.quoted.Type


type AllGenerators = FieldSelectGenerator // & ...

trait GeneratorLoop {
  this: MacroContext
      with DomainObjects
      with AllGenerators => 

  import macroContext.reflect._

  def generateCode[From: Type](actions: List[FocusAction]): FocusResult[Term] = {
    val idOptic: FocusResult[Term] = Right('{Iso.id[From]}.asTerm)
    
    actions.foldLeft(idOptic) { (resultSoFar, action) => 
      resultSoFar.flatMap(term => composeOptics(term, generateActionCode(action)))
    }
  }

  private def generateActionCode(action: FocusAction): Term = 
    action match {
      case FocusAction.Field(name, typeInfo) => generateLens(name, typeInfo)
      //case FocusAction.Attempt => '{ ??? }.asTerm
      //case FocusAction.Index(idx) => '{ ??? }.asTerm
    }

  private def composeOptics(lens1: Term, lens2: Term): FocusResult[Term] = {
    (lens1.tpe.asType, lens2.tpe.asType) match {
      case ('[Lens[from1, to1]], '[Lens[from2, to2]]) =>
        Right('{ 
          ${lens1.asExprOf[Lens[from1, to1]]}.andThen(${lens2.asExprOf[Lens[to1, to2]]})
        }.asTerm)
      case ('[Iso[from1, to1]], '[Lens[from2, to2]]) =>
        Right('{
          ${lens1.asExprOf[Iso[from1, to1]]}.andThen(${lens2.asExprOf[Lens[to1, to2]]})
        }.asTerm)
      case ('[a], '[b]) => FocusError.ComposeMismatch(TypeRepr.of[a].show, TypeRepr.of[b].show).asResult
    }
  }
}