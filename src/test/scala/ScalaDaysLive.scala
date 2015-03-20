import org.scalatest.FunSuite
import play.api.libs.json._
import java.util.Date


case class Email(
  body: String,
  sent: Date
)
case class FullEmail(
  subject: String,
  body: String,
  received: Date,
  sent: Date
)

import org.cvogt.compossible._
import org.cvogt.compossible.conversions._

class ScalaDaysLiveTest extends FunSuite {
  println("-------------------\n\n")

  val jsonString = """
  {
      "name": "Chris",
      "addresses": [
        {"city": "New York", "zip": 10005, "street": "48 Wall St"},
        {"city": "Lausanne", "zip": 1015, "street": "station 14"}
      ],
      "age": 99,
      "SSN": "55555-555-5555"
  }"""

  val json = Json.parse(jsonString)
  val r = json.as[Record[{
    def age: Int
    def addresses: List[Record[{
      def zip: Int
      def street: String
    }]]
  }]]
  println(r.addresses(1).street)

  println("\n\n-------------------")
}
