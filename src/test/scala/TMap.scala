package org.cvogt.test.compossible

import org.scalautils.TypeCheckedTripleEquals._
import org.scalatest.FunSuite

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

import org.cvogt.compossible.TMap

// try lookup in a function

class TMapTest extends FunSuite {
  class Foo{
    class Bar(val i: Int)
  }
  val foo1 = new Foo
  val foo2 = new Foo
  val b1 = new foo1.Bar(9)
  val b2 = new foo2.Bar(10)
  val m = TMap(1) ++
          TMap(2) ++
          TMap("a") ++
          TMap(Option[Int](5)) ++
          TMap(Option("b")) ++
          TMap(b1) ++ 
          TMap(b2)

  test("basic"){
    assert("a" === m[String])
    assert(1   === m[Int])
  }
  test("Generics"){
    assert("b" === m[Option[String]].get)
    assert(5   === m[Option[Int]].get)
  }

  test("path dependend"){
    assert(9 === m[foo1.Bar].i)
    assert(10 === m[foo2.Bar].i)
  }

  /*
  def typeUpcast = {
    def method1(tmap: TMap[Int with String]) = {
      println("doo")
    }
    def method2(tmap: TMap[Int]) = {
      
    }
    /*
    method1(
      TMap(1) ++ TMap("a") ++ TMap(1.1)
    )*/
    val m = TMap(1) ++ TMap("a") ++ TMap(1.1)
    val mf: TMap[Int with String] => Unit = x => println("doo")
    mf(m)
    method1(m)
    method1(TMap(1) ++ TMap("a") ++ TMap(1.1))
    method2(
      TMap(1) ++ TMap("a") ++ TMap(1.1)
    )
/*
    method1(
      TMap(1) ++ TMap("a")
    )
    method2(
      TMap(1) ++ TMap("a")
    )
    method2(
      TMap(1)
    )*/
    1===1
  }
  */
}
