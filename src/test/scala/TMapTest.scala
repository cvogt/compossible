package org.cvogt.test.compossible

import org.scalautils.TypeCheckedTripleEquals._
import org.scalatest.FunSuite

import org.cvogt.compossible.TMap
import scala.reflect.runtime.universe._

class NativeTest extends FunSuite{
  test("basic"){
    assert(5 === TMap(5).apply[Int])
    val t1 = TMap(5).apply[Int]
    assert(5 === t1)

    assert("test" === TMap("test").apply[String])
    val t2 = TMap("test").apply[String]
    assert("test" === t2)
  }

  test("duplicate elements get last inserted"){
    val t3 = TMap(5) ++ TMap(6)
    assert(6 == t3[Int])
    assert(6 == t3[AnyVal])
    assert(6 == t3[Any])

    val t4 = TMap(6) ++ TMap(5)
    assert(5 == t4[Int])
    assert(5 == t4[AnyVal])
    assert(5 == t4[Any])
  }

  case class Name(value: String)
  case class Age(value: Int)

  case class Model(value: String)
  test("test struct"){
    type Person = TMap[Name with Age]
    val p: Person = TMap(Name("Chris")) ++ TMap(Age(99))
    assert("Chris" === p[Name].value)
    assert(99 === p[Age].value)

    // subtyping property TMaps rely on for reader
    implicitly[TMap[Model with Age with Name] <:< TMap[Model] with TMap[Name with Age]]
  }

  case class _1(value: String)
  case class _2(value: Int)
  test("test tuple"){
    type Tuple = TMap[_1 with _2]
    val p: Tuple = TMap(_1("Chris")) ++ TMap(_2(99))
    assert("Chris" === p[_1].value)
    assert(99 === p[_2].value)
  }

  case class left(value: Int)
  case class right(value: Int)
  test("test named arguments"){
    def plus(args: TMap[left with right])
      = args[left].value + args[right].value
    
    assert{
      val args = TMap(left(4)) ++ TMap(right(5))
      9 === plus(args)
    }
  }    

  trait Database{ def query: Any }
  trait Logger  { def log:   Any }
  test("dependency injection test"){
    implicit class FunctionReaderMonad[-T,+R](f: TMap[T] => R){
      def map[Q](g: R => Q) = (t: TMap[T]) => g(f(t))
      def flatMap[S,Q](g: R => (TMap[S] => Q)) = (ts: TMap[T with S]) => g(f(ts))(ts)
    }
    def use[T:TypeTag,R](f: T => R): TMap[T] => R = (c: TMap[T]) => f(c[T])

    val f: TMap[Database with Logger] => Unit
      = for{
        _ <- use{db: Database => db.query}
        _ <- use{logger: Logger => logger.log}
      } yield ()
  }
}
