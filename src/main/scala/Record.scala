package org.cvogt.compossible

import scala.language.experimental.macros
import scala.language.dynamics
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import scala.reflect.macros.blackbox.{Context => BlackboxContext}

/*
class IsNotStruct[T]
object IsNotStruct{
  implicit def i: IsNotStruct[V] = macro isNotStructMacro[V]
}
class IsNotStructMacros(c: BlackboxContext){
  def isNotStructMacro[V:c.WeakTypeTag] = 
}
*/
// Extensible records for Scala based on intersection types

object Record extends Dynamic{
  def structural[V <: AnyRef](struct: V): Record[V]
    = macro RecordBlackboxMacros.createMacro[V]

  def applyDynamic[K <: String,V](key: K)(value: V)/*(implicit ev: IsNotStruct[V])*/: Record[AnyRef]
    = macro RecordWhiteboxMacros.createMacro[K]

  def tuple(record: Record[_]): Product
    = macro RecordWhiteboxMacros.tupleMacro

  def fromCaseClass(obj: Product): Record[AnyRef]
    = macro RecordWhiteboxMacros.fromCaseClassMacro

  def applyDynamicNamed[T <: AnyRef](method: String)(keyValues: T*): Record[AnyRef] = macro RecordWhiteboxMacros.createManyMacro

  //def updateDynamic[K <: String](key: K)(value:Any): Any = macro createMacro2[K]
}

// TODO make invariant and use implicit conversion macro freeing items from internal map
class Record[+T <: AnyRef](
  val values: Map[String, Any],
  val struct: Any = null
) extends Dynamic{
  override def toString = "Record("+values.toString+")"

  /** Combine two records into one.
      The combined one will have the keys of both. */
  def &[O <: AnyRef](other: Record[O])
    = new Record[T with O](values ++ other.values)

  def selectDynamic[K <: String](key: K): Any
    = macro RecordWhiteboxMacros.lookupMacro[K]

  def apply[K <: String](select: select[K]): Record[AnyRef]
    = macro RecordWhiteboxMacros.selectMacro[K]

  def updateDynamic[K <: String](key: K)(value:Any): Record[T]
    = new Record[T](values ++ Map(key -> value))

  def applyDynamic[K <: String](key: K)(value:Any): Record[AnyRef]
    = macro RecordWhiteboxMacros.appendFieldMacro[K]

  //def applyDynamicNamed[K <: String](key: K)(value:Any): Record[_] = macro RecordWhiteboxMacros.
/*
  def selectDynamic[K <: String](k: K): Any
    = macro Record.extractMacro[K]

  def value: Any
    = macro Record.valueMacro
*/
  //def updateDynamic[K <: String](key: K)(value:Any): Any = macro Record.createMacro3[K]
}
trait RecordMacroHelpers extends MacroHelpers{
  val c: BlackboxContext
  import c.universe._

  protected def lookup(record: Tree, key: Tree, valueType: Type)
    = q"$record.values($key).asInstanceOf[$valueType]"

  protected def byKey(typePairs: Seq[Type])
    = typePairs.map(p => splitPair(p)._1 -> p).toMap

  protected def keyValues(record: Tree)
    = splitRefinedTypes(firstTypeArg(record))

  //@scala.annotation.tailrec
  final def collectScopes(tpe: Type, seq: Seq[Symbol] = Seq()): Seq[Symbol] = {
    def println(str: Any) = ()
    println(":"+tpe)
    tpe match {
      case RefinedType(Seq(tpe),scope) => println("1");collectScopes(tpe,seq++scope.toSeq)
      case RefinedType(Seq(),scope) => println("2");scope.toSeq
      case RefinedType(tpes,scope) if scope.isEmpty => println("3");tpes.map(collectScopes(_)).reduce(_ ++ _)
      //case RefinedType(foo,scope) => println((foo,scope));???
      //case tq"AnyRef" => Seq()
      //case other => println("other: "+other.getClass);???
      case other =>
      println("4");
        println(other)
        println(showRaw(other))
        println(other.getClass)
        seq
    }
  }


  protected def keyValues2(record: Tree): Map[String, Type]
    = {/*println(firstTypeArg(record).getClass)
      println(firstTypeArg(record))
      println(showRaw(firstTypeArg(record)))
         */ 
          collectScopes(firstTypeArg(record))
          // match {
        //case typeArg@RefinedType(_,scope) if !scope.isEmpty =>
          //scope
          .map{case s:MethodSymbol => (s.name.decodedName.toString, s.returnType)}
        /*case typeArg =>
          splitRefinedTypes(typeArg).map(splitPair).map{
            case (ConstantType(Constant(key:String)), value) => (key, value)
          }*/
      //})
.toMap
        }

  protected def newRecord(tpe: Tree, keyValues: Seq[Tree], n: Any = null)
    = q"""new Record[$tpe](Map(..$keyValues))"""

  protected def keyString(key: Tree) =
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

  protected def createRecord(_keyValues: (Tree, Tree)*)
    = {
      val keyValues: Seq[(String,Tree)] = _keyValues match {
        case Seq((Literal(Constant("apply")),q"new {..$fields}")) =>
          fields.map{
            case q"def $key = $value" => (key.decodedName.toString,value)
            case q"val $key = $value" => (key.decodedName.toString,value)
          }
        case _ => _keyValues.map{case (Literal(Constant(key: String)),v) => (key, v)}

      }
      val (types, data) = keyValues.map{
        case (key,value) => (
          q"def ${TermName(key)}: ${value.tpe.widen}",
          q"${key} -> ${value}"
        )
      }.unzip
      newRecord(
        tq"AnyRef{..$types}",
        data
      )
    }
}
class RecordBlackboxMacros(val c: BlackboxContext) extends RecordMacroHelpers{
  import c.universe._

  //def createStructuralMacro[K](struct: Tree)
  //  = createRecord((Literal(Constant("apply")), struct))

  def createMacro[K](struct: Tree)
    = createRecord((Literal(Constant("apply")), struct))
}
class RecordWhiteboxMacros(val c: WhiteboxContext) extends RecordMacroHelpers{
  import c.universe._

  def createMacro[K](key: Tree)(value: Tree)
    = createRecord((key, value))

  def selectMacro[K <: String:c.WeakTypeTag]
                 (select: Tree)
    = {
      val selectedTypes = splitRefinedTypes(c.weakTypeTag[K].tpe).map{
        case ConstantType(Constant(key: String)) => key
      }
      println(selectedTypes)

      val typesByKey = keyValues2(c.prefix.tree)
      val defs = selectedTypes zip selectedTypes.map(typesByKey) map {
        case (key, tpe) => q"def ${TermName(key)}: ${tpe}"
      }
      newRecord(
        tq"AnyRef{..$defs}",
        selectedTypes.map{
          key => q"$key -> ${c.prefix}.values($key)"
        }
      )
    }


  def appendFieldMacro[K <: String:c.WeakTypeTag](key: Tree)(value: Tree)
    = {
    keyString(key)
    q"""${c.prefix.tree} & ${createRecord((key, value))}"""
  }

  def createManyMacro(method: Tree)(keyValues: Tree*)
    = createRecord(keyValues.map(splitTreePair):_*)

  def lookupMacro[K <: String:c.WeakTypeTag](key: Tree)
    = {
      //println(firstTypeArg(c.prefix.tree))
      //println(collectScopes(firstTypeArg(c.prefix.tree)))
      //println(key)
      val valueType = 
        keyValues2(c.prefix.tree)
          .get(key match {case Literal(Constant(key:String)) => key})//.tpe match {case ConstantType(Constant(key: String)) => key})
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

      val defs = names.map{
        case (name,tpe) => (
          q"""def ${TermName(name)}: $tpe"""
        )
      }

      newRecord(tq"{..$defs}", keyValues)
    }
}
