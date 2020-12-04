import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day4Test extends AnyFlatSpec with should.Matchers {
  "segment" should "break a list into spanning segments" in {
    val l = List("hej", "", "pa", "", "dig", "san")
    Day4.segment(l, _.isEmpty) shouldEqual (List(
      List("hej"),
      List("pa"),
      List("dig", "san")
    ))
  }
}
