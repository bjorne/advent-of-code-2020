import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day7Test extends AnyFlatSpec with should.Matchers {
  "regex multi matching" should "be something I know how to do in Scala" in {
    val str =
      "drab lime bags contain 5 posh brown bags, 5 muted indigo bags."
    val r =
      raw"(?:(.+)(?= bags contain)|(\d+) (\w+ \w+)(?= bags?))+".r.unanchored
    r.findAllIn(str)
      .toList shouldEqual (List("drab lime", "5 posh brown", "5 muted indigo"))
  }
}
