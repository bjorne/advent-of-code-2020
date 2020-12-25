import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day23Test extends AnyFlatSpec with should.Matchers {
  "answer" should "give numbber for 10 rounds" in {
    Day23.answer(List("389125467"), 10) shouldEqual "92658374"
  }
  "answer" should "give numbber for 100 rounds" in {
    Day23.answer(List("389125467")) shouldEqual "67384529"
  }
  "answer2" should "give numbber for 10000000 rounds" in {
    Day23.answer2(List("389125467")) shouldEqual 934001L * 159792L
  }
}
