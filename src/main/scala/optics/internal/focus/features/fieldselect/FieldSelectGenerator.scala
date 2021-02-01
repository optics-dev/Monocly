package optics.internal.focus.features.fieldselect

import optics.internal.focus.{MacroContext, DomainObjects}
import scala.quoted.Quotes
import optics.poly.Lens


trait FieldSelectGenerator {
  this: MacroContext
    with DomainObjects => 

  import macroContext.reflect._

  def generateLens(field: String, typeInfo: TypeInfo): Term = {
    (typeInfo.from.asType, typeInfo.to.asType) match {
      case ('[f], '[t]) => 
        '{
          val setter: t => f => f = (to: t) => (from: f) => 
            ${ generateSetter(field, '{from}.asTerm, '{to}.asTerm, typeInfo).asExprOf[f] }

          val getter: f => t = (from: f) => 
            ${ generateGetter(field, '{from}.asTerm).asExprOf[t] }

          Lens.apply[f, t](getter, setter)
        }.asTerm
    }
  }

  private def generateGetter(field: String, from: Term): Term = 
    Select.unique(from, field) // o.field

  private def generateSetter(field: String, from: Term, to: Term, typeInfo: TypeInfo): Term = {
    Select.overloaded(from, "copy", typeInfo.fromTypeArgs, NamedArg(field, to) :: Nil) // o.copy(field = value)
  }
}