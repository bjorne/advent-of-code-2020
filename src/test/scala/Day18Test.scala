import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day18Test extends AnyFlatSpec with should.Matchers {
  "answer" should "sum" in {
    Day18.answer(List("1 + 2 * 3 + 4 * 5 + 6")) shouldEqual 71
  }

  "answer2" should "sum" in {
    Day18.answer2(List("2 * 3 + (4 * 5)")) shouldEqual 46
  }
}
