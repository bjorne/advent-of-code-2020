import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day13Test extends AnyFlatSpec with should.Matchers {
  val lines = List("939", "7,13,x,x,59,x,31,19")

  "answer" should "give product" in {
    Day13.answer(lines) shouldEqual (59 * 5)
  }

  "answer2" should "give product" in {
    val lines2 = List("x", "17,x,13,19")
    Day13.answer2(lines2) shouldEqual 3417
  }

  "answer2" should "give right answer" in {
    Day13.answer2(lines) shouldEqual 1068781
  }
}
