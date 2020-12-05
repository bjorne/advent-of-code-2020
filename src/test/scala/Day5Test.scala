import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day5Test extends AnyFlatSpec with should.Matchers {
  "answer" should "give maximum seat ID" in {
    Day5.answer(List("BFFFBBFRRR")) shouldEqual 567
  }
}
