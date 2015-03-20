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
      import org.cvogt.compossible.conversions._
      {
        val r = R.from(p)      
        assert("Chris" === r.name)
        assert(99 === r.age)
      };{
        val r = p ++ R(new{def dob = new java.util.Date})
        assert("Chris" === r.name)
        assert(99 === r.age)
        identity(r.dob: java.util.Date)
      };{
        type Person = R[{
          def name: String
          def age: Int
        }]
        val person = R.named(name="Chris", age=99)
        val r = person With R.named(dob = new java.util.Date)
  //      val r: Person with     {def dob: java.util.Date}
  //           = person With new {def dob= java.util.Date}

        //RecordType name[String] & age[String] & dob[Age]
        //val r: {def name: String; def age: Int; def dob: Date} = p & new{def dob = new java.util.Date}

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

}/*
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
    /*
    };{

      type FooArgs = Record[{name: String; age: Int}]
      def foo(r: FooArgs) = s"${r.name} is ${r.age} years old"

      def bar(s: FooArgs with Record{address: String})
        = foo(s) + " and lives at "+address
    };{

      type FooArgs = {def name: String; def age: Int}
      def foo(r: FooArgs) = s"${r.name} is ${r.age} years old"

      type BarArgs = {address: String}
      def bar(r: FooArgs with BarArgs)
        = foo(r) + " and lives at " + r.address*/
    };{
      type FooArgs = {def name: String; def age: Int}
      case class Foo(r: FooArgs){
        override def toString = s"${r.name} is ${r.age} years old"
      }      
    };{
      type FooArgs = {def name: String; def age: Int}
      class Foo(r: FooArgs){
        override def toString = s"${r.name} is ${r.age} years old"
      }

      type BarArgs = {def address: String}
      class Bar(r: FooArgs with BarArgs) extends Foo(r){
        override def toString = super.toString + " and lives at " + r.address
      }
    };{
      type Foo = {def name: String; def age: Int}
      @inline
      def Foo(r: Foo) = new FooMethods(r)
      implicit class FooMethods(val r: Foo) extends AnyVal{
        import r._
        def string = s"${name} is ${age} years old"
      }

      type Bar = Foo{def address: String}
      implicit class BarMethods(val r: Bar) extends AnyVal{
        def string = Foo(r).string + " and lives at " + r.address
      }
      assert(
        "Chris is 99 years old and lives at NYC" ===
        new {def address = "NYC"; def name= "Chris"; def age=99}.string
      )
    };{
      type Foo = Record[{def name: String; def age: Int}]
      @inline
      def Foo(r: Foo) = new FooMethods(r)
      implicit class FooMethods(r: Foo){
        import r._
        def string = s"${name} is ${age} years old"
      }

      type Bar = Foo with Record[{def address: String}]
      implicit class BarMethods(r: Bar) extends FooMethods(r){
        def string = super.string + " and lives at " + r.address
      }
      assert(
        "Chris is 99 years old and lives at NYC" ===
        new {def address = "NYC"; def name= "Chris"; def age=99}.string
      )
*/
    }
  }
}