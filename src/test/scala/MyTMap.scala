import org.scalatest.FunSuite
import scala.reflect.runtime.universe._

class TMap[+T](val values: List[(Type, Any)]){
  def apply[Q >: T](implicit tt: TypeTag[Q]) = {
    values.find(_._1 <:< tt.tpe).get._2.asInstanceOf[Q]
  }

  def ++[Q](other: TMap[Q]) = {
    new TMap[T with Q](values ++ other.values)
  }
}
object TMap{
  def apply[T](value: T)(implicit tt: TypeTag[T]): TMap[T] = {
    new TMap[T](List(tt.tpe -> value))
  }
}

class Logger{
  def log(msg: String) = println(msg)
}
class Database{
  def query(sql: String) = List((1,"Chris"))
}

class MyTMapLiveTest extends FunSuite {
  implicit class Reader[-T,+R](f: TMap[T] => R){
    def map[Q](g: R => Q): TMap[T] => Q = m => g(f(m))
    def flatMap[S,Q](g: R => (TMap[S] => Q)): TMap[T with S] => Q
      = m => g(f(m))(m)
  }
  test("my TMap"){ 
    println("-------------------\n\n")

    val m = TMap("Chris") ++ TMap(5)
    val s: String = m[String]
    val i: Int    = m[Int]
    assert("Chris" === s)
    assert(5       === i)

    def use[T:TypeTag,R](f: T => R): TMap[T] => R = (m: TMap[T]) => f(m[T])
    def m1 = use{db: Database => db.query("SELECT * FROM person")}
    def m2 = use{l: Logger => l.log("log message")}

    val deps = TMap(new Logger) ++ TMap(new Database)

    val m12 = for{
      people <- m1
      _ <- m2
    } yield people

    println(m12(deps))

    println("\n\n-------------------")
  }
}
