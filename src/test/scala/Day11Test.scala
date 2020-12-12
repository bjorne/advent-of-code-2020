import Day11.Coord
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day11Test extends AnyFlatSpec with should.Matchers {
  val lines = List(
    "#.LL.L#.##",
    "#LLLLLL.L#",
    "L.L.L..L..",
    "#LLL.LL.L#",
    "#.LL.LL.LL",
    "#.LLLL#.##",
    "..L.L.....",
    "#LLLLLLLL#",
    "#.LLLLLL.L",
    "#.#LLLL.##"
  )

  "nextState" should "next state" in {
    val newLines = List(
      "#.##.L#.##",
      "#L###LL.L#",
      "L.#.#..#..",
      "#L##.##.L#",
      "#.##.LL.LL",
      "#.###L#.##",
      "..#.#.....",
      "#L######L#",
      "#.LL###L.L",
      "#.#L###.##"
    )
    Day11.nextState(Day11.parse(lines)) shouldEqual Day11.parse(newLines)
  }

  "isOccpupied" should "count adjacent" in {
    Day11.isOccupied(Day11.parse(lines))(Coord(0, 0)) shouldBe true
    Day11.isOccupied(Day11.parse(lines))(Coord(0, 5)) shouldBe false
    Day11.isOccupied(Day11.parse(lines))(Coord(0, 6)) shouldBe true
  }

  "countNeighbors" should "count adjacent" in {
    Day11.countNeighbors(Day11.parse(lines), Coord(0, 0)) shouldEqual 1
    Day11.countNeighbors(Day11.parse(lines), Coord(0, 5)) shouldEqual 1
    Day11.countNeighbors(Day11.parse(lines), Coord(2, 2)) shouldEqual 0
  }

  "answer" should "iterate until equilibrium" in {
    Day11.answer(Day11.parse(lines)) shouldEqual 37
  }

  val lines2 = List(
    "#.L#.##.L#",
    "#L#####.LL",
    "L.#.#..#..",
    "##L#.##.##",
    "#.##.#L.##",
    "#.#####.#L",
    "..#.#.....",
    "LLL####LL#",
    "#.L#####.L",
    "#.L####.L#"
  )

  "nextState2" should "next state" in {
    val newLines = List(
      "#.L#.L#.L#",
      "#LLLLLL.LL",
      "L.L.L..#..",
      "##LL.LL.L#",
      "L.LL.LL.L#",
      "#.LLLLL.LL",
      "..L.L.....",
      "LLLLLLLLL#",
      "#.LLLLL#.L",
      "#.L#LL#.L#"
    )
    Day11.nextState2(Day11.parse(lines2)) shouldEqual Day11.parse(newLines)
  }

  "countNeighborDirs" should "count adjacent" in {
    Day11.countNeighborDirs(Day11.parse(lines2), Coord(0, 0)) shouldEqual 1
    Day11.countNeighborDirs(Day11.parse(lines2), Coord(0, 2)) shouldEqual 4
    Day11.countNeighborDirs(Day11.parse(lines2), Coord(2, 2)) shouldEqual 5
  }

  "answer2" should "iterate until equilibrium" in {
    Day11.answer2(Day11.parse(lines2)) shouldEqual 26
  }
}
