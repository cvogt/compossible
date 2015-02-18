package org.cvogt.compossible
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

trait MacroHelpers{
  val c: Context
  import c.universe._

  protected def error(msg: String) = c.error(c.enclosingPosition, msg)

  protected def firstTypeArg(tree: Tree) = {
    tree.tpe.widen.dealias.typeArgs.head
  }
  
  protected def prefixTypeArg
    = firstTypeArg(c.prefix.tree)

  protected def splitRefinedTypes(t: Type): Seq[Type] = t match {
    case RefinedType(types,scope) => types.map(splitRefinedTypes(_)).flatten
    case t => Seq(t)
  }

  protected def singletonConstant(tpe: Type): Constant
    = tpe match {
        case ConstantType(c:Constant) => c
      }

  protected def splitPair(t: Type) = {
    val args = t.typeArgs
    (args(0),args(1))
  }

  protected def intersectTypes(types: Seq[Type]): Type
    = internal.refinedType( // TODO: can this be done with Quasi-Quotes?
        types.toList, internal.newScopeWith()
      )
}
