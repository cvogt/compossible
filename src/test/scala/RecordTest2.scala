package org.cvogt.test.records

import org.scalautils.TypeCheckedTripleEquals._
import org.scalatest.FunSuite
//import org.cvogt.compossible.AutoRecords
import org.cvogt.compossible.{Record => R}
//import Predef.{ArrowAssoc => _,Map,identity,println}
case class Person(name: String, age: Int)
case class PersonWithDob(name: String, age: Int, dob: java.util.Date)
class RecordTest2 extends FunSuite {
  test("basic") {
    {
      val p = new Person("Chris", 99)
      import org.cvogt.compossible.syntax._
      {
        val r = R.from(p)      
        assert("Chris" === r.name)
        assert(99 === r.age)
      };{
        val r = p With R(new{def dob = new java.util.Date})
        assert("Chris" === r.name)
        assert(99 === r.age)
        identity(r.dob: java.util.Date)
      };{
  //      val r: Person with     {def dob: java.util.Date}
  //           = person With new {def dob= java.util.Date}

        val r = p ++ new{def dob = new java.util.Date}
        assert("Chris" === r.name)
        assert(99 === r.age)
        identity(r.dob: java.util.Date)

        val r2 = r.to[PersonWithDob]
        assert("Chris" === r2.name)
        assert(99 === r2.age)
        identity(r2.dob: java.util.Date)

        val p2: PersonWithDob = r
        assert("Chris" === p2.name)
        assert(99 === p2.age)
        identity(p2.dob: java.util.Date)

        def foo(p: PersonWithDob) = {
          identity(p.name)
          p
        }
        val p3 = foo(r)
        p3.dob
      };
      {
        val r = R.named(dob = new java.util.Date)
        //assert("Chris" === r.name)
        //assert(99 === r.age)
        identity(r.dob: java.util.Date)
      };
    };
    {
      import org.cvogt.compossible.syntax._
      val r = R(new{def name = "Chris"; def age = 99; def dob = new java.util.Date})
      val p2: PersonWithDob = r
    };{
      import scala.language.experimental.macros
      import org.cvogt.compossible.syntax.foo
      type P = {
        def name: String
      }
      val p = foo

      assert("Chris" === p.name)

      def bar(p: P) = p.name
      // assert("Chris" === bar(p))
    }
  }
}
