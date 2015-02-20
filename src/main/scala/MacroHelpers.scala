package org.cvogt.compossible
import collection.immutable.ListMap
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

trait MacroHelpers{
  val c: Context
  import c.universe._
  val prefixTree = c.prefix.tree

  protected def error(msg: String) = c.error(c.enclosingPosition, msg)
  protected def abort(msg: String) = c.abort(c.enclosingPosition, msg)
  protected def constantString(t: Tree) = t match {
    case Literal(Constant(str: String)) => str
  }
  protected def constantTypeString(t: Type) = t match {
    case ConstantType(Constant(str: String)) => str
  }

  protected def firstTypeArg(tree: Tree) = {
    tree.tpe.widen.dealias.typeArgs.head.dealias
  }

  def typeTree[T: c.WeakTypeTag] = TypeTree(tpe[T])
  def tpe[T: c.WeakTypeTag] = c.weakTypeTag[T].tpe
  def defTree  = (key: String, tpe: Type) => q"def ${TermName(key)}: ${tpe}"
  def defAssignTree  = (key: String, value: Tree) => q"def ${TermName(key)} = ${value}"
  def pairTree = (key: String, value: Tree) => q"$key -> $value"
  def constant = (key: String) => Literal(Constant(key))

  protected def prefixTypeArg
    = firstTypeArg(c.prefix.tree)

  protected def splitRefinedTypes(t: Type): Seq[Type] = t match {
    case RefinedType(types,scope) => types.map(splitRefinedTypes(_)).flatten
    case t => Seq(t)
  }

  protected def splitTreePair(t: Tree) = {
    t match {
      case q"($key, $value)" => (key, value)
    }
  }

  protected def intersectTypes(types: Seq[Type]): Type
    = internal.refinedType( // TODO: can this be done with Quasi-Quotes?
        types.toList, internal.newScopeWith()
      )

  protected def isCaseClass(tpe: Type) = tpe.typeSymbol.asInstanceOf[ClassSymbol].isCaseClass

  protected def caseClassFieldsTypes(tpe: Type) = {
    val params = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    ListMap(params.map{ field =>
      ( field.name.toTermName.decodedName.toString,
        field.typeSignature)
    }: _*)
  }

}
