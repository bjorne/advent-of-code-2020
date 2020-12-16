import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day16Test extends AnyFlatSpec with should.Matchers {
  "answer" should "give invalid scann rate" in {
    val t =
      """class: 1-3 or 5-7
        |row: 6-11 or 33-44
        |seat: 13-40 or 45-50
        |
        |your ticket:
        |7,1,14
        |
        |nearby tickets:
        |7,3,47
        |40,4,50
        |55,2,20
        |38,6,12""".stripMargin
    val lines = t.split("\\n").toList
    Day16.answer(lines) shouldEqual 4 + 55 + 12
  }

  "answer2" should "give invalid scann rate" in {
    val t =
      """class: 0-1 or 4-19
        |row: 0-5 or 8-19
        |seat: 0-13 or 16-19
        |
        |your ticket:
        |11,12,13
        |
        |nearby tickets:
        |3,9,18
        |15,1,5
        |5,14,9""".stripMargin
    val lines = t.split("\\n").toList
    Day16.answer2(lines, prefix = "row") shouldEqual 11
  }
}
