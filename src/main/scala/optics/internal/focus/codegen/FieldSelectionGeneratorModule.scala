package optics.internal.focus.codegen

import optics.internal.focus.{MacroContext, DomainModule}
import scala.quoted.Quotes


trait FieldSelectionGeneratorModule {
  this: MacroContext
      with DomainModule => 

  def generateLens(field: String, typeInfo: TypeInfo)(using Quotes): Term = {
    (typeInfo.from.asType, typeInfo.to.asType) match {
      case ('[f], '[t]) => 
        '{
          val setter = (to: t) => (from: f) => 
            ${ generateSetter(field, '{from}.asTerm, '{to}.asTerm, typeInfo).asExprOf[f] }

          val getter = (from: f) => 
            ${ generateGetter(field, '{from}.asTerm).asExprOf[t] }

          Lens.apply(getter, setter)
        }.asTerm
    }
  }

  private def generateGetter(field: String, from: Term): Term = 
    Select.unique(from, field) // o.field

  private def generateSetter(field: String, from: Term, to: Term, typeInfo: TypeInfo): Term = {
    Select.overloaded(from, "copy", typeInfo.fromTypeArgs, NamedArg(field, to) :: Nil) // o.copy(field = value)
  }
}