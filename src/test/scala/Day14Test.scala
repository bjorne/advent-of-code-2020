import Day14.Mask
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day14Test extends AnyFlatSpec with should.Matchers {
  "Mask" should "apply itself to a number" in {
    val mask = Mask("XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X")
    println("one", mask.one.toBinaryString)
    println("zero", mask.zero.toBinaryString)
    mask(11) shouldEqual 73
    mask(101) shouldEqual 101
    mask(0) shouldEqual 64
  }

  "Mask" should "decode addr" in {
    val mask = Mask("000000000000000000000000000000X1001X")
    mask.decodeAddr(42).toSet shouldEqual Set(26, 27, 58, 59)
  }

  "answer" should "give sum" in {
    val lines = List(
      "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
      "mem[8] = 11",
      "mem[7] = 101",
      "mem[8] = 0"
    )
    Day14.answer(lines, true) shouldEqual 165
  }
}
