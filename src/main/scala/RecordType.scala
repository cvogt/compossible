package org.cvogt.records

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.language.dynamics

// Workaround the current limitation of not being
// able to express singleton types

class RecordType[T <: (String, Any)] extends Dynamic{
  type Type = Record[T]
  def &[Q <: (String, Any)](recordType: RecordType[Q]): Any
    = macro RecordTypeMacros.unionMacro[Q]

  def applyDynamic[V](key: String)
                  (& : org.cvogt.records.&.type)
                  : Any
    = macro RecordTypeMacros.applyUnionMacro[V]

  def selectDynamic[V](key: String): Any
    = macro RecordTypeMacros.selectUnionMacro[V]
}

object RecordType extends Dynamic{
  def applyDynamic[V](key: String)
                  (& : org.cvogt.records.&.type)
                  : Any
    = macro RecordTypeMacros.applyCreateMacro[V]

  def selectDynamic[V](key: String): Any
    = macro RecordTypeMacros.selectCreateMacro[V]
}

class RecordTypeMacros(val c: Context) extends MacroHelpers{
  import c.universe._

  private def create(T: Type,V: Type)
    = q"""new RecordType[($T, $V)]"""

  private def union(Q: Type)
    = q"""new RecordType[$prefixTypeArg with $Q]"""

  private def append(K: Type, V: Type)
    = q"""new RecordType[$prefixTypeArg with ($K, $V)]"""

  def unionMacro[Q <: (String, Any):c.WeakTypeTag](recordType: Tree)
    = union(c.weakTypeTag[Q].tpe)

  def selectUnionMacro[V:c.WeakTypeTag](key: Tree)
    = append(key.tpe,c.weakTypeTag[V].tpe)

  def applyUnionMacro[V:c.WeakTypeTag](key: Tree)(& : Tree)
    = append(key.tpe,c.weakTypeTag[V].tpe)

  def selectCreateMacro[V:c.WeakTypeTag](key: Tree)
    = create(key.tpe,c.weakTypeTag[V].tpe)

  def applyCreateMacro[V:c.WeakTypeTag](key: Tree)(& : Tree)
    = create(key.tpe,c.weakTypeTag[V].tpe)
}
