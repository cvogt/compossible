package final_version
import org.scalatest.FunSuite
import scala.reflect.runtime.universe._
class TMap[+T](val values: Seq[(Type,Any)]){
  def apply[Q >: T](implicit tt: TypeTag[Q]) = {
    values.find(_._1 <:< tt.tpe).get._2.asInstanceOf[Q]
  }
  def ++[Q](other: TMap[Q]): TMap[T with Q] = new TMap[T with Q](values ++ other.values)
}
object TMap{
  def apply[T](value: T)(implicit tt: TypeTag[T]) = {
    new TMap[T](Seq(tt.tpe -> value))
  }
}
class Database{
  def query(sql: String) = List((1,"Chris"))
}
class Logger{
  def log(msg: String) = println(msg)
}
class MyTMapLiveTest extends FunSuite {
  implicit class Reader[-T,+R](f: TMap[T] => R){
    def map[Q](g: R => Q): TMap[T] => Q = m => g(f(m))
    def flatMap[S,Q](g: R => (TMap[S] => Q)): TMap[T with S] => Q
      = m => g(f(m))(m)
  }
  test("my TMap"){  
    val m = TMap("Chris") ++ TMap(5)
    val s: String = m[String]
    val i: Int = m[Int]
    assert("Chris" === s)
    assert(5 === i)

    def use[T:TypeTag,R](f: T => R) = (m: TMap[T]) => f(m[T])

    val m12//: (TMap[Database with Logger]) => List[(Int,String)]
      = for{
        people <- use{db: Database => db.query("SELECT * FROM PERSON")}
        _ <- use{l: Logger => l.log("log message")}
      } yield people
  }
}
