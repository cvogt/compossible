package org.cvogt.compossible

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.language.dynamics

// Extensible records for Scala based on intersection types

object Record extends Dynamic{
  def applyDynamic[K <: String](key: K)(value:Any): Record[(String, Any)]
    = macro RecordMacros.createMacro[K]

  def tuple(record: Record[_]): Product
    = macro RecordMacros.tupleMacro

  def fromCaseClass(obj: Product): Record[(String, Any)]
    = macro RecordMacros.fromCaseClassMacro
  //def updateDynamic[K <: String](key: K)(value:Any): Any = macro createMacro2[K]
}

// TODO make invariant and use implicit conversion macro freeing items from internal map
class Record[+T <: (String,Any)](
  val values: Map[String, Any]
) extends Dynamic{
  override def toString = "Record("+values.toString+")"

  /** Combine two records into one.
      The combined one will have the keys of both. */
  def &[O <: (String,Any)](other: Record[O])
    = new Record[T with O](values ++ other.values)

  def selectDynamic[K <: String](key: K): Any
    = macro RecordMacros.lookupMacro[K]

  def apply[K <: String](select: select[K]): Record[(String, Any)]
    = macro RecordMacros.selectMacro[K]

  def updateDynamic[K <: String](key: K)(value:Any): Record[T]
    = new Record[T](values ++ Map(key -> value))

  def applyDynamic[K <: String](key: K)(value:Any): Record[(String, Any)]
    = macro RecordMacros.appendFieldMacro[K]

  //def applyDynamicNamed[K <: String](key: K)(value:Any): Record[_] = macro RecordMacros.
/*
  def selectDynamic[K <: String](k: K): Any
    = macro Record.extractMacro[K]

  def value: Any
    = macro Record.valueMacro
*/
  //def updateDynamic[K <: String](key: K)(value:Any): Any = macro Record.createMacro3[K]
}

class RecordMacros(val c: Context) extends MacroHelpers{
  import c.universe._

  private def lookup(record: Tree, key: Tree, valueType: Type)
    = q"$record.values($key).asInstanceOf[$valueType]"

  private def byKey(typePairs: Seq[Type])
    = typePairs.map(p => splitPair(p)._1 -> p).toMap

  private def keyValues(record: Tree)
    = splitRefinedTypes(firstTypeArg(record))

  private def newRecord(tpe: Tree, keyValues: Seq[Tree])
    = q"""new Record[$tpe](Map(..$keyValues))"""

  private def keyString(key: Tree) =
    key match{
      case Literal(Constant("apply")) =>
        error("Error: .apply is prohibited for Records.")
        ???
        //q"""${c.prefix}"""
      case Literal(Constant(const)) => const
      case _ =>
        error("Only string literals are allows as keys, not: "+key)
        ???
    }

  def createRecord(key: Tree, value: Tree)
    = newRecord(
        tq"(${key.tpe}, ${value.tpe.widen})",
        Seq(q"${key} -> ${value}")
      )

  def selectMacro[K <: String:c.WeakTypeTag]
                 (select: Tree)
    = {
      val selectedTypes = splitRefinedTypes(c.weakTypeTag[K].tpe)

      newRecord(
        TypeTree(intersectTypes(
          selectedTypes.map(byKey(keyValues(c.prefix.tree)))
        )),

        selectedTypes.map{
          case ConstantType(key:Constant) => 
            q"$key -> ${c.prefix}.values($key)"
        }
      )
    }


  def appendFieldMacro[K <: String:c.WeakTypeTag](key: Tree)(value: Tree)
    = {
    keyString(key)
    q"""${c.prefix.tree} & ${createRecord(key, value)}"""
  }

  def createMacro[K <: String:c.WeakTypeTag](key: Tree)(value: Tree)
    = createRecord(key, value)

  def lookupMacro[K <: String:c.WeakTypeTag](key: Tree)
    = {
      val valueType = 
        keyValues(c.prefix.tree)
          .map(splitPair)
          .toMap
          .get(key.tpe)
          .getOrElse{
            error(s"""Record has no key .${keyString(key)}""")
            ???
          }
      lookup(c.prefix.tree, key, valueType)
    }
/*
  def valueMacro: c.Expr[Any]
    = {
      import c.universe._
      val recordTypeArg = c.prefix.actualType.widen.typeArgs.head

      def splitTypes(t: Type): Seq[Type] = t match {
        case RefinedType(types,scope) => types.map(splitTypes(_)).flatten
        case t => Seq(t)
      }
      val keyValuePairTypes: Seq[Type] = splitTypes(recordTypeArg)
      if(keyValuePairTypes.size >1){
        c.error(
          c.enclosingPosition,
          ".value can only be called on single-element Records"
        )
      }

      val Seq(ConstantType(key:Constant),v) = keyValuePairTypes.head.typeArgs
      c.Expr[Any](q"""${c.prefix}.values($key).asInstanceOf[$v]""")
    }

  def extractMacro[K <: String:c.WeakTypeTag]
    (k: c.Expr[K]): c.Expr[Any]
    = {
      val recordTypeArg = c.prefix.actualType.widen.typeArgs.head

      def splitTypes(t: Type): Seq[Type] = t match {
        case RefinedType(types,scope) => types.map(splitTypes(_)).flatten
        case t => Seq(t)
      }
      val keyValuePairTypes: Seq[Type] = splitTypes(recordTypeArg)

      val keyValueMap = keyValuePairTypes.map{
        t =>
          val args = t.typeArgs
          (args(0),args(1))
      }.toMap

      val keyString = k.tree match{
        case Literal(Constant("apply")) => c.error(
          c.enclosingPosition, 
          "Error: You are trying to use \"apply\" as Record key or call a Record's apply method. Both are prohibited."
        )
        case Literal(Constant(s)) => s
        case _ => c.error(c.enclosingPosition, "Only string literals are allows as keys, not: "+k.tree)
      }
      val kt = c.weakTypeTag[K]
      val v = keyValueMap.get(k.tree.tpe).getOrElse{
        c.error(
          c.enclosingPosition,
          s"""Record has no key .$keyString"""
        )
        ???
      }
      c.Expr[Any](q"""new Record[(${k.tree.tpe},$v)](Map(${k.tree} -> ${c.prefix}.values(${k.tree}).asInstanceOf[$v]))""")
    }
*/
  def tupleMacro(record: Tree)
    = {
      val accessors = 
        keyValues(record)
          .map(splitPair)
          .map{
            case (ConstantType(key:Constant),valueType) => 
              lookup(record,Literal(key),valueType)
          }
      q"""(..$accessors)"""
    }

  def fromCaseClassMacro(obj: Tree)
    = {
      val tpe = obj.tpe.widen.dealias
      assert(tpe.typeSymbol.asInstanceOf[ClassSymbol].isCaseClass)

      val params =
        tpe.decls.collectFirst {
          case m: MethodSymbol if m.isPrimaryConstructor => m
        }.get.paramLists.head
      
      val names = params.map{ field =>
        ( field.name.toTermName.decodedName.toString,
          field.typeSignature)
      }

      val keyValues = names.map{
        case (name,tpe) => (
          q"""${Constant(name)} -> ${obj}.${TermName(name)}"""
        )
      }

      val types = names.map{
        case (name,tpe) => (
          tq"""(${internal.constantType(Constant(name))},${tpe})"""
        )
      }.reduce((a,b) => tq"""$a with $b""")

      newRecord(types, keyValues)
    }
}
