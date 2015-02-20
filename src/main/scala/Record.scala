package org.cvogt.compossible

import collection.immutable.ListMap
import scala.language.experimental.macros
import scala.language.dynamics
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import scala.reflect.macros.blackbox.{Context => BlackboxContext}

// Extensible records for Scala based on intersection types
object RecordCompletion{
  import scala.language.implicitConversions
  /** import for IntelliJ code completion
      Instead of whitebox macros selectDynamic member resolution,
      switches to structural type record member resolution, which
      uses reflection and leads to corresponding warnings.
      Suggestion: Use this during development and remove for production. */
  implicit def unpack[T](record: Record[T]): T = macro RecordBlackboxMacros.unpack[T]  
}
trait RecordFactory extends Dynamic{
  /** [whitebox] Create a record from a case class */
  def from(obj: Any): Record[AnyRef]
    = macro RecordWhiteboxMacros.from
  /** [whitebox] Create a record from a named arguments list */
  def applyDynamicNamed[T <: AnyRef](method: String)(keysValues: T*): Record[AnyRef]
    = macro RecordWhiteboxMacros.applyDynamicNamed
}
/** [whitebox] Create a record from a named arguments list 
    (same as Record.named but allows renaming) */
object RecordNamed extends RecordFactory

/** [blackbox] Create a record from a structural refinement type (new { ... })*/
object Record extends RecordFactory{
  def apply[V <: AnyRef](struct: V): Record[V]
    = macro RecordBlackboxMacros.apply[V]

  implicit class RecordMethods[T <: AnyRef](val record: Record[T]){
    val values = record.values
    /** [whitebox] Convert Record into a tuple */
    def toTuple: Product
      = macro RecordWhiteboxMacros.tuple    

    /** like structural upcasting but removes the values of the lost fields from memory */
    def select[S >: T <: AnyRef]: Record[S] = macro RecordBlackboxMacros.select[S]

    def to[S]: S = macro RecordBlackboxMacros.to[S]

    /** Combine two records into one.
        The combined one will have the keys of both. */
    def &[O <: AnyRef](other: Record[O])
      = new Record[T with O](values ++ other.values)

    def With[O <: AnyRef](other: Record[O])
      = new Record[T with O](values ++ other.values)

    def apply[K <: String](select: select[K]): Record[AnyRef]
      = macro RecordWhiteboxMacros.selectWithSelector[K]

    /** select columns */
    // def map[E](f: T => E)

  }
}

// Record is co-variant for structural upcasting
// For memory efficient conversion use .select instead
class Record[+T <: AnyRef](
  val values: Map[String, Any],
  val struct: Any = null // FIXME
) extends Dynamic{
  override def toString = "Record("+values.toString+")"

  def selectDynamic[K <: String](key: K): Any
    = macro RecordWhiteboxMacros.lookup[K]

  def applyDynamic[K <: String](key: K)(value:Any): Record[AnyRef]
    = macro RecordWhiteboxMacros.appendField[K]

  // TODO: make typesafe with macros
  def applyDynamicNamed(method: String)(keyValues: (String, Any)*): Record[T]
    = macro RecordBlackboxMacros.copy[T]

  // somehow conflicted with applyDynamic, etc.
  //def updateDynamic[K <: String](key: K)(value:Any): Record[T]
  //  = new Record[T](values ++ Map(key -> value))

  //def updateDynamic[K <: String](key: K)(value:Any): Any = macro Record.create3[K]
}
trait RecordMacroHelpers extends MacroHelpers{
  val c: BlackboxContext
  import c.universe._

  /** new record from key values pairs */ 
  protected def newRecord(tpe: Tree, keyValues: Tree, n: Any = null)
    = q"""new Record[$tpe]($keyValues)"""

  /** lookupTree record values */
  protected def lookupTree(record: Tree, key: String, valueType: Type)
    = q"$record.values(${constant(key)}).asInstanceOf[$valueType]"

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

  /** map from field names to types */
  protected def extractTypesByKey(tpe: Type): Map[String, Type]
    = ListMap(
        collectDefs(tpe).map{
          case s:MethodSymbol =>
            (s.name.decodedName.toString, s.returnType)
        }: _*
      )

  protected def createRecord(keysValues: Seq[(String, Tree)]) = {
    val defs = keysValues.map{
      case (key,value) =>
        q"def ${TermName(key)}: ${value.tpe.widen}"
    }
    val data = keysValues.map{
      case (key, value) => q"$key -> $value"
    }
    newRecord( tq"AnyRef{..$defs}", q"Map(..$data)" )
  }

  protected def selectHelper(selectedKeys: Seq[String]) = {
    val allTypesByKey      = extractTypesByKey(firstTypeArg(prefixTree))

    val defs   = selectedKeys zip selectedKeys.map(allTypesByKey) map defTree.tupled
    val values = selectedKeys map (key => (key,q"$prefixTree.values($key)")) map pairTree.tupled

    newRecord( tq"AnyRef{..$defs}", q"Map(..$values)" )
  }

}
class RecordBlackboxMacros(val c: BlackboxContext) extends RecordMacroHelpers{
  import c.universe._

  def apply[K](struct: Tree) =
    createRecord(
      struct match {
        case q"new {..$fields}" =>
          fields.map{
            case q"def $key = $value" => (key.decodedName.toString,value)
            case q"val $key = $value" => (key.decodedName.toString,value)
          }
      }
    )

  def select[K:c.WeakTypeTag] = {
    val allTypesByKey      = extractTypesByKey(firstTypeArg(prefixTree))
    val selectedTypesByKey = extractTypesByKey(tpe[K])
    selectedTypesByKey.foreach{
      case (key, tpe) if tpe == allTypesByKey(key) =>
    }
    val selectedKeys = selectedTypesByKey.keys.toSeq
    selectHelper(selectedKeys)
  }

  /** create a new structural refinement type for the data of the record */
  def unpack[T:c.WeakTypeTag](record: Tree): Tree = {
    //q"org.cvogt.compossible.RecordLookup($record)"
    val accessors = extractTypesByKey(firstTypeArg(record)).map{
      case(key, tpe) => defAssignTree(key, lookupTree(record, key, tpe))
    }
    q"new{..$accessors}"
  }

  def copy[T:c.WeakTypeTag](method: Tree)(keyValues: Tree*) = {
    method match {
      case Literal(Constant("copy")) =>
      case _ => error("no method found ${constantString(method)}")
    }
    newRecord(typeTree[T], q"$prefixTree.values ++ Map(..$keyValues)")
  }

  def to[K:c.WeakTypeTag] = {
    val typesByKey = extractTypesByKey(firstTypeArg(prefixTree))

    tpe[K] match {
      case tpe if isCaseClass(tpe) =>
        val accessors = caseClassFieldsTypes(tpe).map(_._1).map{
          case key => lookupTree(prefixTree,key,typesByKey(key))
        }
        q"""new $tpe(..$accessors)"""
    }
  }
}

case class RecordLookup(r: Record[_ <: AnyRef]) extends Dynamic{
  def selectDynamic[T](key: String) = r.values(key).asInstanceOf[T]
}
class RecordWhiteboxMacros(val c: WhiteboxContext) extends RecordMacroHelpers{
  import c.universe._

  def selectWithSelector[K <: String:c.WeakTypeTag](select: Tree)
    = selectHelper(
        splitRefinedTypes(tpe[K]).map(constantTypeString)
      )

  def applyDynamicNamed(method: Tree)(keysValues: Tree*)
    = createRecord(
        keysValues.map(splitTreePair).map{
          case (keyTree, value) => (constantString(keyTree), value)
        }
      )


  def appendField[K <: String:c.WeakTypeTag](key: Tree)(value: Tree) = {
    val record = createRecord(Seq((constantString(key), value)))
    q"$prefixTree With $record"
  }

  def lookup[K <: String:c.WeakTypeTag](key: Tree)
    = {
      val keyString = constantString(key)
      val valueType = 
        extractTypesByKey(firstTypeArg(prefixTree))
          .get(keyString)
          .getOrElse{
            error(s"""Record has no key .${key}""")
            ???
          }
      lookupTree(prefixTree, keyString, valueType)
    }
/*
  def value: c.Expr[Any]
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

  def extract[K <: String:c.WeakTypeTag]
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
  def tuple
    = {
      val accessors = 
        extractTypesByKey(firstTypeArg(prefixTree)).map{
            case (key,valueType) => 
              lookupTree(q"$prefixTree.record",key,valueType)
          }
      q"""(..$accessors)"""
    }

  def from(obj: Tree) = {
    val tpe = obj.tpe.widen.dealias
    assert(isCaseClass(tpe))

    val fieldsTypes = caseClassFieldsTypes(tpe) 
    val keyValues = fieldsTypes.keys.map(k => (k,q"$obj.${TermName(k)}")).map(pairTree.tupled)

    val defs = fieldsTypes map defTree.tupled

    newRecord(tq"{..$defs}", q"Map(..$keyValues)")
  }
}
