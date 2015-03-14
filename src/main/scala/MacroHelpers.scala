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
  def pairTree = (key: String, value: Tree) => q"($key, $value)"
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

  protected def isCaseClass(tpe: Type)
    = tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass


  /*protected def isStructuralRefinementType(obj: Tree)
    = obj match {
        case q"new{...}" => true // FIXME: this is too imprecise
        case _ => false
      }*/
  protected def isStructuralRefinementType(obj: Type) = {
    obj match {
      case RefinedType(List(TypeRef(ThisType(pkgSym), aliasSym, List())), _)
        if pkgSym.name == TypeName("scala")
           && aliasSym.name == TypeName("AnyRef")
         => true
      case TypeRef(NoPrefix, classSym, List())
        if classSym.name == TypeName("$anon") => true
      case _ => false
    }
  }

  protected def caseClassFieldsTypes(tpe: Type): Map[String, Type] = {
    val params = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    ListMap(params.map{ field =>
      ( field.name.toTermName.decodedName.toString,
        field.typeSignature)
    }: _*)
  }

  /** Collects all definition symbols from the the refinement bodies
      of the involved types.
      E.g. Seq(def name: String, def age:Int)
      for  {def name: String} with {def age: Int}
      
      Warning: not tail recursive, could blow the stack */
  protected final def collectDefs(tpe: Type, seq: Seq[Symbol] = Seq()): Seq[Symbol] = {
    tpe match {
      case RefinedType(Seq(tpe),scope) => collectDefs(tpe,seq++scope.toSeq)
      case RefinedType(Seq(),scope) => scope.toSeq
      case RefinedType(tpes,scope) if scope.isEmpty => tpes.map(collectDefs(_)).reduce(_ ++ _)
      case other => seq
    }
  }

  protected final def collectRefinedTypes(tpe: Type): Seq[Type] = {
    tpe match {
      case RefinedType(types,_) => types.map(collectRefinedTypes).flatten
      case other => Seq(other)
    }
  }
}
