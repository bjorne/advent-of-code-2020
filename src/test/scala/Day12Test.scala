import Day12.{Coord, State}
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day12Test extends AnyFlatSpec with should.Matchers {
  "State.moveForward" should "move in given direction" in {
    State(Coord(0, 0), 0).moveForward(1).c shouldEqual Coord(1, 0)
    State(Coord(0, 0), 90).moveForward(1).c shouldEqual Coord(0, 1)
    State(Coord(0, 0), 180).moveForward(1).c shouldEqual Coord(-1, 0)
    State(Coord(0, 0), 270).moveForward(1).c shouldEqual Coord(0, -1)
  }
  "answer" should "give distance" in {
    val lines = List("F10", "N3", "F7", "R90", "F11")
    Day12.answer(Day12.parse(lines)) shouldEqual 17 + 8
  }

  "rotateCoord" should "rotate coord around origin" in {
    Day12.rotateCoord(Coord(1, 0), 90) shouldEqual Coord(0, 1)
    Day12.rotateCoord(Coord(1, 0), -90) shouldEqual Coord(0, -1)
    Day12.rotateCoord(Coord(15, 4), 90) shouldEqual Coord(-4, 15)
  }

  "answer2" should "give distance" in {
    val lines = List("F10", "N3", "F7", "R90", "F11")
    Day12.answer2(Day12.parse(lines)) shouldEqual 286
  }
}
