package org.cvogt.compossible

import collection.immutable.ListMap
import scala.language.experimental.macros
import scala.language.dynamics
import scala.reflect.macros.whitebox.{Context => WhiteboxContext}
import scala.reflect.macros.blackbox.{Context => BlackboxContext}
import scala.reflect.runtime.{universe => refl}
import org.cvogt.scala.constraint._

/**
see scala-extensions
*/
class :=[T,Q]
trait Default_:={
  /** Ignore default */
  implicit def useProvided[Provided,Default] = new :=[Provided,Default]
}
object := extends Default_:={
  /** Infer type argument to default */
  implicit def useDefault[Default] = new :=[Default,Default]
}
/*
object Foo{
  import reflect._
  def foo[T](implicit default: T := AnyRef, ct: ClassTag[T]) = ct.toString
}
*/
// Extensible records for Scala based on intersection types
object RecordCompletion{
  import scala.language.implicitConversions
  /** import for IntelliJ code completion
      Instead of whitebox macros selectDynamic member resolution,
      switches to structural type record member resolution, which
      uses reflection and leads to corresponding warnings.
      Suggestion: Use this during development and remove for production. */
  implicit def unpack[T](record: Record[T]): T = macro RecordWhiteboxMacros.unpack[T]  
  implicit def foo[T <: AnyRef](d: Option[Double]): T = macro RecordWhiteboxMacros.foo
}
// TODO: implicit conversion to structural type with macro members for reflection free access
object syntax{
  import scala.language.implicitConversions

  /** Validates if type is convertable */
  class ConvertibleToRecord[T]
  implicit def ConvertibleToRecord[T]: ConvertibleToRecord[T]
    = macro syntaxMacros.assertConvertibleToRecord[T]
  
  class InferReturnType[T]{
    def cast(a: Any) = a.asInstanceOf[T]
  }

  implicit def InferReturnType[T]: InferReturnType[T]
    = macro syntaxMacros.inferReturnType[T]
  
  class ConvertibleFromRecord[T]
  implicit def ConvertibleFromRecord[T]: ConvertibleFromRecord[T]
    = macro syntaxMacros.assertConvertibleFromRecord[T]

  type IsRecord[T] = ConvertibleToRecord[T] // FIXME

  /**
  ConvertibleToRecord is neede, because failing the implicit
  conversion does not continue implicit search, but failing
  an implicit parameter does */
  implicit def ToRecordMethods[T:ConvertibleToRecord](t: T)
                                 : Record.RecordMethods[AnyRef]
                                 = macro syntaxMacros.recordMethods

  implicit def ToRecord[T:ConvertibleToRecord](t: T)
                                 : Record[AnyRef]
                                 = macro syntaxMacros.record

  implicit def RecordTo
    [R,U]
    (t: R)
    (implicit inferReturnType: InferReturnType[U])//InferReturnType[R,U])
    : U
    = macro syntaxMacros.recordTo[U]

    def foo: {
      def name: String
    } = macro syntaxMacros.fooImpl
}


class syntaxMacros(val c: WhiteboxContext) extends RecordWhiteboxMacrosTrait{
  import c.universe._
  def recordMethods(t: Tree)(ri: Tree) = {
    q"new $RecordCompanion.RecordMethods(${from(t)})"
  }

  def nameImpl = Literal(Constant("Chris"))
  def fooImpl = q"""new {
    def name: String = macro $pkg.syntaxMacros.nameImpl
  }"""

  def inferReturnType[T:c.WeakTypeTag] = {
    try{
      import c._
      val tpe = weakTypeTag[T].tpe.dealias
      //println("wt:"+tpe.dealias.typeSymbol.name)
      //println(openImplicits)
      val returnType =
        if(tpe.typeSymbol.name == TypeName("T")){      
          openImplicits.map{
            case i@ImplicitCandidate(_, _, tpe, targetValue)
              =>
              //println("X:"+tpe.dealias);
              tpe.dealias
          }.collect {
            case TypeRef(pkg, fnSymbol, args :: returnType :: Nil)
              //if isCaseClass(returnType) // <- TODO remove
              //if pkg.typeSymbol.name == TypeName("scala")
                 //&& fnSymbol.name == TypeName("Function1")
              => returnType
          }.last
        } else {
          tpe
        }
        /** Prevent "undetermined type" compiler crash, see
        https://github.com/scala/scala/blob/3d76836bc81c3ec183e83ee186e447ff212507d0/src/compiler/scala/tools/nsc/typechecker/Infer.scala#L106 */
        def acceptableReturnType(t: Type): Boolean = t match {
          case WildcardType | BoundedWildcardType(_) | NoType => false
          case RefinedType(types,_) => types.forall(acceptableReturnType)
          case _ => true
        }
        assert(acceptableReturnType(returnType))
//        println(returnType.getClass)
      //println("returnType: "+returnType)
      q"new $pkg.syntax.InferReturnType[$returnType]"
    } catch {
      case scala.util.control.NonFatal(e) =>
        //e.printStackTrace(System.out)
        throw e
    }
  }

  def record(t: Tree)(ri: Tree) = {
    //println(t.tpe.widen.dealias)
    from(t)
  }
  def recordTo[U: c.WeakTypeTag](t: Tree)(inferReturnType: Tree) = {
    //println("U: "+tpe[U])
    //${from(t)}
    val target = to2[U](t)
    //println(target)
    q"$inferReturnType.cast($target)"
  }
  def assertConvertibleFromRecord[T:c.WeakTypeTag] = {
    val T = tpe[T]
    //println(T)
    q"""new $pkg.syntax.ConvertibleFromRecord[$T]"""
  }
  def assertConvertibleToRecord[T:c.WeakTypeTag] = {
    //println(">")
    val T = tpe[T]
    if(!isConvertibleToRecord[T])
      error(s"$T can't be converted to Record")
    val t = q"""new $pkg.syntax.ConvertibleToRecord[$T]"""
    //println("<")
    t
  }

  def isConvertibleToRecord[T:c.WeakTypeTag] = {
    val T = tpe[T]
    isCaseClass(T) || isStructuralRefinementType(T) || isRecord(T)
  }
  def toRecord = {
    prefixTree match {
      case q"$pkg.syntax.ToRecord($obj)" => from(obj)
      //case other => error(other.toString);???
    }
  }
  /*
  def record2(t: Tree) = {
    q"$RecordCompanion.from($t)"
  }
  def record3(t: Tree) = {
    q"$RecordCompanion($t)"
  }*/

  def to2[K:c.WeakTypeTag](struct: Tree) = {
    weakTypeTag[K].tpe.dealias match {
      case tpe if isCaseClass(tpe) =>
        val fieldsTypes = caseClassFieldsTypes(tpe)
        val values = fieldsTypes.map{
          case (key, tpe) =>
            q"$struct.selectDynamic(${Constant(key)})"
        }
        q"new $tpe(..$values)"
    }
  }
}

//trait Structural
/** [blackbox] Create a record from a structural refinement type (new { ... })*/
object Record extends Dynamic{
  /** [whitebox] Create a record from a case class */
  def from(obj: Any): Any//Record[AnyRef]
    = macro RecordWhiteboxMacros.from

  def applyDynamic[V <: AnyRef](method: String)(struct: V): Record[V]
    = macro RecordBlackboxMacros.applyMacro[V]

  /** [whitebox] Create a record from a named arguments list */
    def applyDynamicNamed[T](method: String)(keysValues: (String, Any)*)(implicit d: T := AnyRef): Record[T]
      = macro RecordWhiteboxMacros.createForType2[T]

  /** [whitebox] Create a record from a named arguments list 
      (object with apply to support named arguments */
  object named extends Dynamic{
    /** [whitebox] Create a record from a named arguments list */
    def applyDynamicNamed[T <: AnyRef](method: String)(keysValues: T*): Record[AnyRef]
      = macro RecordWhiteboxMacros.createFromNamedArguments
  }

  /** [whitebox] Create a record matching the given type
      (object with apply to support named arguments */
  object typed extends Dynamic{
    /** [whitebox] Create a record matching the given type */
    def applyDynamicNamed[T](method: String)(keysValues: Any*): Record[T]
      = macro RecordWhiteboxMacros.createForType[T]
  }

  implicit class RecordMethods[+T <: AnyRef](val record: Record[T]){
    def ++[O:syntax.ConvertibleToRecord](other: O): Record[AnyRef]
      = macro RecordWhiteboxMacros.++[T,O]

    val values = record.values
    /** [whitebox] Convert Record into a tuple */
    def toTuple: Product
      = macro RecordWhiteboxMacros.tuple    

    /** like structural upcasting but removes the values of the lost fields from memory */
    def select[S >: T <: AnyRef]: Record[S] = macro RecordBlackboxMacros.select[S]

    def to[S]: S = macro RecordBlackboxMacros.to[S]

    /** @see With */
    def &[O <: AnyRef](other: Record[O]): Record[T with O]
      = macro RecordWhiteboxMacros.With[T,O]

    /** [blackbox] Combine two records into one.
        The combined one will have the keys of both.

        This actually uses a whitebox to produce a more
        readable but semantically equivalent return type.
                {def foo: A} with {def bar: B}
        becomes {def foo: A; def bar: B}
        It however behaves like a blackbox macro for all purposes.
    */
    def With[O <: AnyRef](other: Record[O]): Record[T with O]
      = macro RecordWhiteboxMacros.With[T,O]

    def apply[K <: String](select: select[K]): Record[AnyRef]
      = macro RecordWhiteboxMacros.selectWithSelector[K]

    /** select columns */
    // def map[E](f: T => E)

  }
}

// Record is co-variant for structural upcasting
// For memory efficient conversion use .select instead
class Record[+T <: AnyRef](
  val values: Map[String, Any]
) extends /*AnyVal with Dynamic*/{
  outer =>
  override def toString = "Record("+values.toString+")"

  def selectDynamic[K <: String](key: K): Any
    = macro RecordWhiteboxMacros.lookup[K]

  /*
  def record = this
  object apply extends Dynamic{
    val record = outer
    def applyDynamic[K <: String](key: K)(value:Any): Record[AnyRef]
      = macro RecordWhiteboxMacros.appendField[K]
  }
  */

  object copy extends Dynamic{
    val record = outer
    // TODO: make typesafe with macros
    def applyDynamicNamed(method: String)(keyValues: (String, Any)*): Record[T]
      = macro RecordBlackboxMacros.copy[T]
  }


  // somehow conflicted with applyDynamic, etc.
  //def updateDynamic[K <: String](key: K)(value:Any): Record[T]
  //  = new Record[T](values ++ Map(key -> value))

  //def updateDynamic[K <: String](key: K)(value:Any): Any = macro Record.create3[K]
}
trait RecordMacroHelpers extends MacroHelpers{
  val c: BlackboxContext
  import c.universe._
  val pkg = q"_root_.org.cvogt.compossible"
  val Record = tq"$pkg.Record"
  val RecordCompanion = q"$pkg.Record"

  /** new record from key values pairs */ 
  protected def newRecord(tpe: Tree, keyValues: Tree, n: Any = null)
    = q"""new $Record[$tpe]($keyValues)"""

  /** lookupTree record values */
  protected def lookupTree(record: Tree, key: String, valueType: Type)
    = q"$record.values(${constant(key)}).asInstanceOf[$valueType]"

  /** map from field names to types */
  protected def extractTypesByKey(tpe: Type): Map[String, Type]
    = ListMap(
        collectDefs(tpe).map{
          case s:MethodSymbol =>
            (s.name.decodedName.toString, s.returnType)
        }: _*
      )

  protected def createRecord(keysValues: Seq[(String, Tree)]) = {
    createRecord2(keysValues.map{
      case (key, value) => (key, value.tpe.widen, value)
    })
  }

  protected def createRecord2(keysValues: Seq[(String, Type, Tree)]) = {
    val defs = keysValues.map{
      case (key, tpe, _) =>
        q"def ${TermName(key)}: ${tpe}"
    }
    val data = keysValues.map{
      case (key, _, value) => q"($key, $value)"
    }
    newRecord( tq"AnyRef{..$defs}", q"Map(..$data)" )
  }

  protected def selectHelper(selectedKeys: Seq[String]) = {
    val allTypesByKey      = extractTypesByKey(firstTypeArg(prefixTree))

    val defs   = selectedKeys zip selectedKeys.map(allTypesByKey) map defTree.tupled
    val values = selectedKeys map (key => (key,q"$prefixTree.values($key)")) map pairTree.tupled

    newRecord( tq"AnyRef{..$defs}", q"Map(..$values)" )
  }

  def applyMacro[K](method: Tree)(struct: Tree) = apply[K](struct)
  def apply[K](struct: Tree) = {
    //println(struct)
    //println(struct.tpe)
    struct match {
      case q"new {..$fields}" =>
        createRecord(fields.map{
          case q"def $key = $value" => (key.decodedName.toString,value)
          case _ => error("Records only support def members when being constructed from an anonymous class");???
        })
      case caseClass if isCaseClass(caseClass.tpe) =>
        val fieldsTypes = caseClassFieldsTypes(caseClass.tpe)
        createRecord2(fieldsTypes.map{
          case (key, tpe) => (key, tpe, q"$caseClass.${TermName(key)}")
        }.toSeq)
      case v if isStructuralRefinementType(v.tpe) =>
        val fieldsTypes = extractTypesByKey(v.tpe)
        createRecord2(fieldsTypes.map{
          case (key, tpe) => (key, tpe, q"$v.${TermName(key)}")
        }.toSeq)
    }
  }
}
class RecordBlackboxMacros(val c: BlackboxContext) extends RecordMacroHelpers{
  import c.universe._

  def select[K:c.WeakTypeTag] = {
    val allTypesByKey      = extractTypesByKey(firstTypeArg(prefixTree))
    val selectedTypesByKey = extractTypesByKey(tpe[K])
    selectedTypesByKey.foreach{
      case (key, tpe) if tpe =:= allTypesByKey(key) =>
    }
    val selectedKeys = selectedTypesByKey.keys.toSeq
    selectHelper(selectedKeys)
  }

  def copy[T:c.WeakTypeTag](method: Tree)(keyValues: Tree*) = {
    /*method match {
      case Literal(Constant("copy")) =>
      case _ => error(s"no method found ${constantString(method)}")
    }*/
    newRecord(typeTree[T], q"$prefixTree.record.values ++ Map(..$keyValues)")
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

class RecordWhiteboxMacros(val c: WhiteboxContext) extends RecordWhiteboxMacrosTrait
trait RecordWhiteboxMacrosTrait extends RecordMacroHelpers{
  import c.universe._

  def createForType[T:c.WeakTypeTag](method: Tree)(keysValues: Tree*) = {
    createRecord{
      val keysValuesSplit = keysValues.map(splitTreePair).map{
        case (keyTree, value) => (constantString(keyTree), value)
      }
      val positionalValues = keysValuesSplit.takeWhile(_._1 == "").map(_._2)
      val positionalKeys = extractTypesByKey(tpe[T].dealias).take(positionalValues.size).keys
      val namedArgs = keysValuesSplit.dropWhile(_._1 == "")
      if(namedArgs.exists(_._1 == "")) error("positional after named argument")
      (positionalKeys zip positionalValues).toSeq ++ namedArgs
    }
  }

  def createForType2[T:c.WeakTypeTag](method: Tree)(keysValues: Tree*)(d: Tree) = {
    if(c.weakTypeOf[T] =:= c.weakTypeOf[AnyRef])
      createFromNamedArguments(method)(keysValues:_*)
    else
      createForType[T](method)(keysValues:_*)
  }

  protected def isRecord(obj: Type) = {
    obj <:< typeOf[Record[_]]
  }

  def foo(d: Tree) = q"""new {def name = "Chris"}"""

  /** create a new structural refinement type for the data of the record */
  def unpack[T:c.WeakTypeTag](record: Tree): Tree = {
    //q"org.cvogt.compossible.RecordLookup($record)"
    val accessors = extractTypesByKey(firstTypeArg(record)).map{
      case(key, tpe) => defAssignTree(key, lookupTree(record, key, tpe))
    }
    q"new{..$accessors}"
  }

  def ++[T:c.WeakTypeTag,O:c.WeakTypeTag](other: Tree)(evidence$5: Tree) = {
    //println(prefixTree)
    q"$prefixTree With ${from(other)}"
  }

  /**
  Merge structural refinement types T and O for better error messages
  */
  def With[T:c.WeakTypeTag,O:c.WeakTypeTag](other: Tree) = {//(evidence$2: Tree)
    val defs = extractTypesByKey(tpe[T]) ++ extractTypesByKey(tpe[O]) map defTree.tupled
    newRecord(tq"{..$defs}", q"$prefixTree.values ++ $other.values")
  }

  def selectWithSelector[K <: String:c.WeakTypeTag](select: Tree)
    = selectHelper(
        splitRefinedTypes(tpe[K]).map(constantTypeString)
      )

  def createFromNamedArguments(method: Tree)(keysValues: Tree*) = {
    createRecord(
      keysValues.map(splitTreePair).map{
        case (keyTree, value) => (constantString(keyTree), value)
      }
    )
  }

  def appendField[K <: String:c.WeakTypeTag](key: Tree)(value: Tree) = {
    val record = createRecord(Seq((constantString(key), value)))
    q"$prefixTree.record With $record"
  }

  def lookup[K <: String:c.WeakTypeTag](key: Tree)
    = {
      val keyString = constantString(key)
      val ident = TermName(keyString)
      q"_root_.org.cvogt.compossible.RecordCompletion.unpack($prefixTree).$ident"
      /*
      val keyString = constantString(key)
      val valueType = 
        extractTypesByKey(firstTypeArg(prefixTree))
          .get(keyString)
          .getOrElse{
            error(s"""Record has no key .${key}""")
            ???
          }
      lookupTree(prefixTree, keyString, valueType)
      */
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
  def tuple = {
    val accessors = 
      extractTypesByKey(firstTypeArg(prefixTree)).map{
          case (key,valueType) => 
            lookupTree(q"$prefixTree.record",key,valueType)
        }
    q"""(..$accessors)"""
  }

  def from(obj: Tree) = {
    (obj,obj.tpe.widen.dealias) match {
      //case RefinedType(_,_) => q"$RecordCompanion($obj)" // FIXME: probably too general
      case (obj,tpe) if isCaseClass(tpe) =>
        val fieldsTypes = caseClassFieldsTypes(tpe) 
        val keyValues = fieldsTypes.keys.map(k => (k,q"$obj.${TermName(k)}")).map(pairTree.tupled)

        val defs = fieldsTypes map defTree.tupled

        newRecord(tq"{..$defs}", q"Map(..$keyValues)")
//      case _ => error("Can't convert to Record: "+obj.toString);???
      case (obj,tpe) if isStructuralRefinementType(tpe) => apply(obj)
      case (obj,tpe) if isRecord(tpe) => obj
      case (obj,RefinedType(List(TypeRef(ThisType(pkgSym), aliasSym, List())), _))
                if pkgSym.name == TypeName("scala")
                   && aliasSym.name == TypeName("AnyRef") => apply(obj)
      case (obj,tpe) => error(s"Can't convert ${showRaw(tpe)} + ${isRecord(tpe)} to Record");???
    }
  }
}
