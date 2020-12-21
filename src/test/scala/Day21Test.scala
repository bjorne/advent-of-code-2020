import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day21Test extends AnyFlatSpec with should.Matchers {
  val lines = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                  |trh fvjkl sbzzf mxmxvkd (contains dairy)
                  |sqjhc fvjkl (contains soy)
                  |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin
    .split("\n")
    .toList

  "answer" should "count ingredients without allergens" in {
    Day21.answer(lines) shouldEqual 5
  }

  "answer2" should "give canonical dangerous ingredient list" in {
    Day21.answer2(lines) shouldEqual "mxmxvkd,sqjhc,fvjkl"
  }
}
