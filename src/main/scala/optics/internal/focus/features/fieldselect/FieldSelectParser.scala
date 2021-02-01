package optics.internal.focus.features.fieldselect

import optics.internal.focus.MacroContext

trait FieldSelectParser {
  this: MacroContext => 

  import this.macroContext.reflect._
  
  def getFieldType(fromType: TypeRepr, fieldName: String): Option[TypeRepr] = {
    fromType.classSymbol.flatMap { 
      _.memberField(fieldName) match {
        case FieldType(possiblyTypeArg) => Some(swapWithSuppliedType(fromType, possiblyTypeArg))
        case _ => None
      }
    }
  }

  def getSuppliedTypeArgs(fromType: TypeRepr): List[TypeRepr] = {
    fromType match {
      case AppliedType(_, argTypeReprs) => argTypeReprs 
      case _ => Nil
    }
  }

  object CaseClass {
    def unapply(term: Term): Option[Term] =
      term.tpe.classSymbol.flatMap { sym => 
        Option.when(sym.flags.is(Flags.Case))(term)
      }
  }

  private object FieldType {
    def unapply(fieldSymbol: Symbol): Option[TypeRepr] = fieldSymbol match {
      case sym if sym.isNoSymbol => None
      case sym => sym.tree match {
        case ValDef(_, typeTree, _) => Some(typeTree.tpe)
        case _ => None
      }
    }
  }

  private def getDeclaredTypeArgs(classType: TypeRepr): List[Symbol] = {
    classType.classSymbol.map(_.primaryConstructor.paramSymss) match {
      case Some(typeParamList :: _) if typeParamList.exists(_.isTypeParam) => typeParamList
      case _ => Nil
    }
  }

  private def swapWithSuppliedType(fromType: TypeRepr, possiblyContainsTypeArgs: TypeRepr): TypeRepr = {
    val declared = getDeclaredTypeArgs(fromType)
    val supplied = getSuppliedTypeArgs(fromType)
    val swapDict = declared.view.map(_.name).zip(supplied).toMap
    
    def swapInto(candidate: TypeRepr): TypeRepr = {
      candidate match {
        // Waiting until we can get an AppliedType constructor
        //case AppliedType(typeCons, args) => AppliedType(swapInto(typeCons), args.map(swapInto))
        case leafType => swapDict.getOrElse(leafType.typeSymbol.name, leafType)
      }
    }
    swapInto(possiblyContainsTypeArgs)
  }


}