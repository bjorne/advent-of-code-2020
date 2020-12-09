import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day9Test extends AnyFlatSpec with should.Matchers {
  val lines = List(35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182,
    127, 219, 299, 277, 309, 576).map(_.toString)

  "answer" should "give first non-sum" in {
    Day9.answer(lines, 5) shouldEqual Some(127)
  }

  "answerContiguous" should "give first non-sum" in {
    Day9.answerContiguous(lines, 5) shouldEqual Some(15 + 47)
  }
}
