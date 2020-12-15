import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day10Test extends AnyFlatSpec with should.Matchers {
  val lines = List(28, 33, 18, 42, 31, 14, 46, 20, 48, 47, 24, 23, 49, 45, 19,
    38, 39, 11, 1, 32, 25, 35, 8, 17, 7, 9, 4, 2, 34, 10, 3).map(_.toString)

  "joltDiffs" should "return 1 and 3 diffs" in {
    Day10.joltDiffs(lines.map(_.toInt)) shouldEqual Seq(22, 10)
  }

  "answer" should "give #1 * #3 jolt diffs" in {
    Day10.answer(lines) shouldEqual (22 * 10)
  }

  val simple = List("16", "10", "15", "5", "1", "11", "7", "19", "6", "12", "4")
  "answer2" should "give simple arrangemnets" in {
    Day10.answer2(simple) shouldEqual 8
  }
  "answer2" should "give arrangemnets" in {
    Day10.answer2(lines) shouldEqual 19208
  }
}
