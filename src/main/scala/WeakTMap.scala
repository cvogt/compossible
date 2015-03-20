package org.cvogt.compossible
import scala.reflect.runtime.universe._

/**
A type-indexed map implementation with no tricks.
You get what the type-system gives you, which already
covers many use cases.
*/
class WeakTMap[+T] private(private val values: List[(Type, Any)]) extends AnyVal{
  def apply[E >: T](implicit tt: TypeTag[E]): E = {
    // runtime reflection doesn't allow constant time element lookup
    values.find(_._1 <:< tt.tpe).get._2.asInstanceOf[E]
  }

  /**
  Concatenate two WeakTMaps.
  Elements of the left one override 
  */
  def ++[S](other: WeakTMap[S])
    = new WeakTMap[T with S](other.values ++ values)

  override def toString = "WeakTMap("+values.map{ case (key,value) => key + " -> "+value}.mkString(", ")+")" // FIXME
}

object WeakTMap {
  def apply[T](values: T)(implicit tt: TypeTag[T]) =
    new WeakTMap[T](List((tt.tpe, values)))
}
