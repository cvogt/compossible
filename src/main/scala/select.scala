package org.cvogt.compossible
import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.language.dynamics

// structural by name selectors that allow
// to describe sub-records for projections

class select[T] extends Dynamic{
  def &[Q](select: select[Q]): Any
    = macro selectMacros.unionMacro[Q]

  def applyDynamic(key: String)
                  (value: org.cvogt.compossible.&.type)
                  : Any
    = macro selectMacros.applyUnionMacro

  def selectDynamic(key: String): Any
    = macro selectMacros.selectUnionMacro
}

object &
object select extends Dynamic{
  def applyDynamic(key: String)
                  (value: org.cvogt.compossible.&.type)
                  : Any
    = macro selectMacros.applyCreateMacro

  def selectDynamic(key: String): Any
    = macro selectMacros.selectCreateMacro
}

class selectMacros(val c: Context) extends MacroHelpers{
  import c.universe._

  private def create(T: Type) = q"""new select[$T]"""

  private def union(Q: Type)
    = q"""new select[$prefixTypeArg with $Q]"""

  def unionMacro[Q:c.WeakTypeTag](select: Tree)
    = union(c.weakTypeTag[Q].tpe)

  def selectUnionMacro(key: Tree)
    = union(key.tpe)

  def applyUnionMacro(key: Tree)(value: Tree)
    = union(key.tpe)

  def selectCreateMacro(key: Tree)
    = create(key.tpe)

  def applyCreateMacro(key: Tree)(value: Tree)
    = create(key.tpe)
}
