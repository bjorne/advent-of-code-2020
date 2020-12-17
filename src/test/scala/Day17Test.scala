import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day17Test extends AnyFlatSpec with should.Matchers {
  "answer" should "give active cubes" in {
    val t =
      """.#.
        |..#
        |###""".stripMargin
    val lines = t.split("\\n").toList
    Day17.answer(lines) shouldEqual 112
  }
}
