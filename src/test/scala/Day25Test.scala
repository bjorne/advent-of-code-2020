import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day25Test extends AnyFlatSpec with should.Matchers {
  "answer" should "establish encryption key" in {
    Day25.answer(Seq("5764801", "17807724")) shouldEqual 14897079
  }
}
