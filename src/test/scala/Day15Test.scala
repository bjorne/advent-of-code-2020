import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day15Test extends AnyFlatSpec with should.Matchers {
  "answerSimple" should "give number" in {
    Day15.answerSimple(List("0,3,6"), 10) shouldEqual 0
    Day15.answerSimple(List("1,3,2")) shouldEqual 1
    Day15.answerSimple(List("2,1,3")) shouldEqual 10
    Day15.answerSimple(List("1,2,3")) shouldEqual 27
    Day15.answerSimple(List("2,3,1")) shouldEqual 78
    Day15.answerSimple(List("3,2,1")) shouldEqual 438
    Day15.answerSimple(List("3,1,2")) shouldEqual 1836
  }

  "answer" should "give number" in {
    Day15.answer(List("0,3,6"), 10) shouldEqual 0
    Day15.answer(List("1,3,2")) shouldEqual 1
    Day15.answer(List("2,1,3")) shouldEqual 10
    Day15.answer(List("1,2,3")) shouldEqual 27
    Day15.answer(List("2,3,1")) shouldEqual 78
    Day15.answer(List("3,2,1")) shouldEqual 438
    Day15.answer(List("3,1,2")) shouldEqual 1836
  }

  "answer" should "give 30 millionth number" in {
    Day15.answer(List("1,3,2"), 30000000) shouldEqual 2578
    Day15.answer(List("0,3,6"), 30000000) shouldEqual 175594
    Day15.answer(List("2,1,3"), 30000000) shouldEqual 3544142
    Day15.answer(List("1,2,3"), 30000000) shouldEqual 261214
    Day15.answer(List("2,3,1"), 30000000) shouldEqual 6895259
    Day15.answer(List("3,2,1"), 30000000) shouldEqual 18
    Day15.answer(List("3,1,2"), 30000000) shouldEqual 362
  }
}
