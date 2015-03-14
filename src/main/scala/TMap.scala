package org.cvogt.compossible
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.{Context => BlackboxContext}

import org.cvogt.scala.constraint.set._
import org.cvogt.scala.constraint._

/**
A type-indexed map implementation with no tricks.
You get what the type-system gives you, which already
covers many use cases.
*/
class TMap[T] private(val _values: Seq[Any]) extends AnyVal{
  def apply[E >: T](
    implicit 
    ev: E NotIn (Any,AnyRef,AnyVal,Nothing),
    ev2: E !=:= Nothing
  ): E = macro TMapMacros.lookup[E]

  /**
  Concatenate two TMaps.
  Elements of the left one override 
  */
  def ++[S](other: TMap[S]) = new TMap[T with S](_values ++ other._values)

  override def toString = "TMap("+_values.mkString(", ")+")"
}

object TMap {
  def apply[T](value: T) = new TMap[T](Seq(value))
}

/*
abstract class TypeHashCode[T](debug: String){ // FIXME: make invariant
  def typeHashCode: Any
}
object TypeHashCode{
  implicit def typeHashCode[T]: TypeHashCode[T] = macro TMapMacros.typeHashCode[T]
}*/

class TMapMacros(val c: BlackboxContext) extends RecordMacroHelpers{
  import c.universe._
  import c.{WeakTypeTag,weakTypeTag}
  def lookup[E:c.WeakTypeTag](ev: Tree, ev2: Tree) = {
    val keys = collectRefinedTypes(firstTypeArg(prefixTree))
    val E = weakTypeOf[E]
    /*
    println("-"*80)
    println(keys)
    println(E)
    */
    assert(!(E =:= weakTypeOf[Nothing]))
    val position = keys.indexWhere(_ =:= E)
    /*
    println(position)
    println("-"*80)
    */
    /*
    q"""
    println($prefixTree._values)
    println($position)
    """*/
    q"""
    $prefixTree._values($position).asInstanceOf[$E]
    """
  }
/*  {
    // runtime reflection doesn't allow constant time element lookup
    println("-"*80)
    println(thc)
    println(values)
    println(thc.typeHashCode)
    collectRefinedTypes
    val v = values(thc.typeHashCode).asInstanceOf[E]
    println("-"*80)
    v
  }
  def deepNormalize(t: Type): Type = t.map(_.dealias) match { case `t` => t; case x => deepNormalize(x) }
  
  def typeHashCode[T:c.WeakTypeTag] = {
    val T = //deepNormalize(
      weakTypeOf[T]
      //.tpe)
    println(T)
    println(showRaw(T))
    println(T.hashCode)
    q"""
    new _root_.org.cvogt.compossible.TypeHashCode[$T](${T.toString}){
      def typeHashCode = ${T.hashCode}
    }
    """
  }*/
}

/*
import scala.reflect.runtime.universe._
import scala.reflect.macros.blackbox.Context
import scala.language.implicitConversions
import scala.language.experimental.macros


/*
TODO
- make lookup constant time (currently broken hashCode String bv. java.lang.String)
  - https://issues.scala-lang.org/browse/SI-5959
  - https://groups.google.com/forum/#!topic/scala-internals/S-9OaYe7aP4
  - https://groups.google.com/forum/#!msg/scala-internals/P2_okWT4muw/-4MnftsRhwMJ
  
- upcast implicit conversion
- safe set union (preventing conflicting keys)
- fix string singleton types (probably false alarm and problem was String bv. java.lang.String)
- more performant macro replacement for typetag
*/
/*
FUTURE WORK: variants

- use string singleton types as keys
  e.g. val m = TMap[("name".type, String) with ("age".type, Int)]
  - insert, merge, applyDynamic macro
    usage: m.name


*/

object SubSetMacro{
  def check[TypeSubSet, TypeSet <: TypeSubSet](c: Context)(implicit 
      typeSubSetUnion: c.WeakTypeTag[TypeSubSet], 
      typeSetUnion: c.WeakTypeTag[TypeSet]): c.Expr[SubSet[TypeSubSet, TypeSet]] = {

    import c.universe._

    def extractTypeElements[T](w: Type): Set[Type] = (w match {
      case RefinedType(s, _) => s.toSet.flatMap(extractTypeElements(_:Type))
      case a => Set(a)
    }).map(_.widen.dealias.etaExpand)//.map{t => println(showRaw(t)); t}

    val subSet = extractTypeElements(typeSubSetUnion.tpe)
    val set = extractTypeElements(typeSetUnion.tpe)

    if(!(subSet subsetOf set)) c.abort(c.enclosingPosition, 
      set +" is not a " + subSet + "  " + typeSetUnion + "is not a " + typeSubSetUnion
    )

    c.Expr[SubSet[TypeSubSet, TypeSet]](q"null")
  }
}

object SubSet {
  implicit def foo[TypeSubSet, TypeSet <: TypeSubSet]: SubSet[TypeSubSet, TypeSet] = macro SubSetMacro.check[TypeSubSet, TypeSet]
}
sealed trait SubSet[TypeSubSet, TypeSet <: TypeSubSet]

class TMap[T] private(private val a: Map[Symbol, Any]) {
  override def toString = a.toString
  def ++[S](o: TMap[S]) = new TMap[T with S](o.a ++ a)
  def lookup[L >: T](implicit tt: TypeTag[L], ev: L SubSet T): L = {
    //println(TMap.deepNormalize(tt.tpe))
    //println(a.keys.map(TMap.deepNormalize))
    a(TMap.deepNormalize(tt.tpe.dealias).asInstanceOf[TypeRef].sym).asInstanceOf[L]
    // a.find((x:(Type,Any)) => x._1 =:= tt.tpe).get._2.asInstanceOf[L]
  }
  def lookupConstant[L >: T](implicit tt: TypeTag[L], ev: L SubSet T): L = {
    //println(TMap.deepNormalize(tt.tpe))
    //println(a.keys.map(TMap.deepNormalize))
    a(TMap.deepNormalize(tt.tpe.dealias).asInstanceOf[TypeRef].sym).asInstanceOf[L]
  }
}
object TMap {
  def deepNormalize(t: Type): Type = t.map(_.widen.dealias.etaExpand) match { case `t` => t; case x => deepNormalize(x) }
  def apply[T](a: T)(implicit tt: TypeTag[T]) =
    new TMap[T](Map(deepNormalize(tt.tpe.dealias).asInstanceOf[TypeRef].sym -> a))
    //implicit def upcast[T,Q >: T](tmap: TMap[T])(implicit ev1: Q !=:= AnyVal,ev: Q SubSet T) = tmap.asInstanceOf[TMap[Q]]
}

/*
TMap[A with B with C] => TMap[T] where T in powerset({A,B,C})

someList.find(...) == Some("test")

List(1, 3, 3).any(_ == 1)

*/
*/