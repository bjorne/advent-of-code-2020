import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day8Test extends AnyFlatSpec with should.Matchers {
  "mutate" should "do a mutation on an index" in {
    val code = List(("acc", 4), ("nop", -5), ("jmp", -1))
    Day8.mutateIndex(code, 1) shouldEqual Some(
      List(("acc", 4), ("jmp", -5), ("jmp", -1))
    )
    Day8.mutateIndex(code, 0) shouldEqual None
  }
}
