package org.cvogt.test.compossible

import org.scalautils.TypeCheckedTripleEquals._
import org.scalatest.FunSuite

import org.cvogt.compossible.WeakTMap
import scala.reflect.runtime.universe._

class WeakTMapTest extends FunSuite{
  test("basic"){
    assert(5 === WeakTMap(5).apply[Int])
    val t1 = WeakTMap(5).apply[Int]
    assert(5 === t1)

    assert("test" === WeakTMap("test").apply[String])
    val t2 = WeakTMap("test").apply[String]
    assert("test" === t2)
  }

  test("duplicate elements get last inserted"){
    val t3 = WeakTMap(5) ++ WeakTMap(6)
    assert(6 == t3[Int])
    assert(6 == t3[AnyVal])
    assert(6 == t3[Any])

    val t4 = WeakTMap(6) ++ WeakTMap(5)
    assert(5 == t4[Int])
    assert(5 == t4[AnyVal])
    assert(5 == t4[Any])
  }

  case class Name(value: String)
  case class Age(value: Int)

  case class Model(value: String)
  test("test struct"){
    type Person = WeakTMap[Name with Age]
    val p: Person = WeakTMap(Name("Chris")) ++ WeakTMap(Age(99))
    assert("Chris" === p[Name].value)
    assert(99 === p[Age].value)

    // subtyping property WeakTMaps rely on for reader
    implicitly[WeakTMap[Model with Age with Name] <:< WeakTMap[Model] with WeakTMap[Name with Age]]
  }

  case class _1(value: String)
  case class _2(value: Int)
  test("test tuple"){
    type Tuple = WeakTMap[_1 with _2]
    val p: Tuple = WeakTMap(_1("Chris")) ++ WeakTMap(_2(99))
    assert("Chris" === p[_1].value)
    assert(99 === p[_2].value)
  }

  case class left(value: Int)
  case class right(value: Int)
  test("test named arguments"){
    def plus(args: WeakTMap[left with right])
      = args[left].value + args[right].value
    
    assert{
      val args = WeakTMap(left(4)) ++ WeakTMap(right(5))
      9 === plus(args)
    }
  }    

  trait Database{ def query: Any }
  trait Logger  { def log:   Any }
  test("dependency injection test"){
    implicit class FunctionReaderMonad[-T,+R](f: WeakTMap[T] => R){
      def map[Q](g: R => Q) = (t: WeakTMap[T]) => g(f(t))
      def flatMap[S,Q](g: R => (WeakTMap[S] => Q)) = (ts: WeakTMap[T with S]) => g(f(ts))(ts)
    }
    def use[T:TypeTag,R](f: T => R): WeakTMap[T] => R = (c: WeakTMap[T]) => f(c[T])

    val f
      = for{
        _ <- use{db: Database => db.query}
        _ <- use{logger: Logger => logger.log}
      } yield ()

    val f2: WeakTMap[Database with Logger] => Unit = f
  }
}
class Foo{
  class Bar(val i:Int)
}
class WeakTMap2 extends FunSuite {
  val foo1 = new Foo
  val foo2 = new Foo
  val b1 = new foo1.Bar(5)
  val b2 = new foo2.Bar(10)

  val m = WeakTMap(1) ++
          WeakTMap("a") ++
          WeakTMap(Option(5)) ++
          WeakTMap(Option("a")) ++
          WeakTMap(b1) ++ 
          WeakTMap(b2)
  test("basic"){
    assert("a" === m[String])
    assert(1   === m[Int])
  }
  test("Generics"){
    assert("a" === m[Option[String]].get)
    assert(5   === m[Option[Int]].get)
  }

  test("path dependend"){
    assert(5  === m[foo1.Bar].i)
    assert(10 === m[foo2.Bar].i)
  }
}