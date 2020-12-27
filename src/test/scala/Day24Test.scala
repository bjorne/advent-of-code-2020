import Day24.Coord
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day24Test extends AnyFlatSpec with should.Matchers {
  "Coord" should "walk consistently" in {
    Coord(0, 0).walk("se").walk("w").walk("ne") shouldEqual Coord(0, 0)
    Coord(0, 0).walk("se").walk("nw") shouldEqual Coord(0, 0)
    Coord(0, 0).walk("sw").walk("ne") shouldEqual Coord(0, 0)
    Coord(0, 0)
      .walk("nw")
      .walk("w")
      .walk("sw")
      .walk("e")
      .walk("e") shouldEqual Coord(0, 0)

  }

  "answer" should "count flipped simple" in {
    Day24.answer(Seq("nw")) shouldEqual 1
    Day24.answer(Seq("nw", "nw")) shouldEqual 0
    Day24.answer(Seq("nwse", "senw")) shouldEqual 0
  }

  val lines = """sesenwnenenewseeswwswswwnenewsewsw
                  |neeenesenwnwwswnenewnwwsewnenwseswesw
                  |seswneswswsenwwnwse
                  |nwnwneseeswswnenewneswwnewseswneseene
                  |swweswneswnenwsewnwneneseenw
                  |eesenwseswswnenwswnwnwsewwnwsene
                  |sewnenenenesenwsewnenwwwse
                  |wenwwweseeeweswwwnwwe
                  |wsweesenenewnwwnwsenewsenwwsesesenwne
                  |neeswseenwwswnwswswnw
                  |nenwswwsewswnenenewsenwsenwnesesenew
                  |enewnwewneswsewnwswenweswnenwsenwsw
                  |sweneswneswneneenwnewenewwneswswnese
                  |swwesenesewenwneswnwwneseswwne
                  |enesenwswwswneneswsenwnewswseenwsese
                  |wnwnesenesenenwwnenwsewesewsesesew
                  |nenewswnwewswnenesenwnesewesw
                  |eneswnwswnwsenenwnwnwwseeswneewsenese
                  |neswnwewnwnwseenwseesewsenwsweewe
                  |wseweeenwnesenwwwswnew""".stripMargin.split("\n").toSeq

  "answer" should "count flipped" in {
    Day24.answer(lines) shouldEqual 10
  }

  "answer2" should "simulate floor" in {
    Day24.answer2(lines, 10) shouldEqual 37
    Day24.answer2(lines) shouldEqual 2208
  }
}
