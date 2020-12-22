import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day22Test extends AnyFlatSpec with should.Matchers {
  val lines =
    """Player 1:
      |9
      |2
      |6
      |3
      |1
      |
      |Player 2:
      |5
      |8
      |4
      |7
      |10""".stripMargin
      .split("\n")
      .toList

  "answer" should "score winning plager" in {
    Day22.answer(lines, false) shouldEqual 306
  }
  "answer2" should "score winning plager" in {
    Day22.answer(lines, true) shouldEqual 291
  }

  "answer2" should "quick exits on infinite loop" in {
    val lines =
      """Player 1:
    |43
    |19
    |
    |Player 2:
    |2
    |29
    |14""".stripMargin.split("\n").toList
    Day22.answer(lines, true) shouldEqual 1
  }
}
