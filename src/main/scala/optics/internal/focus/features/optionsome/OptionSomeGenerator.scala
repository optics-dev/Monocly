package optics.internal.focus.features.optionsome

import optics.internal.focus.FocusBase

private[focus] trait OptionSomeGenerator {
  this: FocusBase => 

  import macroContext.reflect._

  def generateOptionSome(toType: TypeRepr): Term = {
    toType.asType match {
      case '[t] => '{ _root_.optics.poly.Prism.some[t] }.asTerm
    }
  }
}