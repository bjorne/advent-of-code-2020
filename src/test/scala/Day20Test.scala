import Day20._
import org.scalatest.flatspec._
import org.scalatest.matchers._

class Day20Test extends AnyFlatSpec with should.Matchers {
  "Coord#rotate" should "rotate given steps" in {
    val t = Tile(1, Set(Coord(0, 0)), 2)
    t.rotateLeft(-1).filled shouldEqual Set(Coord(1, 0))
    t.rotateLeft(1).filled shouldEqual Set(Coord(0, 1))
    t.rotateLeft(2).filled shouldEqual Set(Coord(1, 1))
    t.rotateLeft(3).filled shouldEqual Set(Coord(1, 0))
    t.rotateLeft(4).filled shouldEqual Set(Coord(0, 0))
  }

  lazy val lines =
    """Tile 2311:
      |..##.#..#.
      |##..#.....
      |#...##..#.
      |####.#...#
      |##.##.###.
      |##...#.###
      |.#.#.#..##
      |..#....#..
      |###...#.#.
      |..###..###
      |
      |Tile 1951:
      |#.##...##.
      |#.####...#
      |.....#..##
      |#...######
      |.##.#....#
      |.###.#####
      |###.##.##.
      |.###....#.
      |..#.#..#.#
      |#...##.#..
      |
      |Tile 1171:
      |####...##.
      |#..##.#..#
      |##.#..#.#.
      |.###.####.
      |..###.####
      |.##....##.
      |.#...####.
      |#.##.####.
      |####..#...
      |.....##...
      |
      |Tile 1427:
      |###.##.#..
      |.#..#.##..
      |.#.##.#..#
      |#.#.#.##.#
      |....#...##
      |...##..##.
      |...#.#####
      |.#.####.#.
      |..#..###.#
      |..##.#..#.
      |
      |Tile 1489:
      |##.#.#....
      |..##...#..
      |.##..##...
      |..#...#...
      |#####...#.
      |#..#.#.#.#
      |...#.#.#..
      |##.#...##.
      |..##.##.##
      |###.##.#..
      |
      |Tile 2473:
      |#....####.
      |#..#.##...
      |#.##..#...
      |######.#.#
      |.#...#.#.#
      |.#########
      |.###.#..#.
      |########.#
      |##...##.#.
      |..###.#.#.
      |
      |Tile 2971:
      |..#.#....#
      |#...###...
      |#.#.###...
      |##.##..#..
      |.#####..##
      |.#..####.#
      |#..#.#..#.
      |..####.###
      |..#.#.###.
      |...#.#.#.#
      |
      |Tile 2729:
      |...#.#.#.#
      |####.#....
      |..#.#.....
      |....#..#.#
      |.##..##.#.
      |.#.####...
      |####.#.#..
      |##.####...
      |##..#.##..
      |#.##...##.
      |
      |Tile 3079:
      |#.#.#####.
      |.#..######
      |..#.......
      |######....
      |####.#..#.
      |.#...#.##.
      |#.#####.##
      |..#.###...
      |..#.......
      |..#.###...""".stripMargin.split("\n").toList

  "answer" should "give product of ids of corner tiles" in {
    Day20.answer(lines) shouldEqual 1951L * 3079L * 2971L * 1171L
  }

  "reconstruct and weld" should "give the right thingn" in {
    val welded =
      """.#.#..#.##...#.##..#####
        |###....#.#....#..#......
        |##.##.###.#.#..######...
        |###.#####...#.#####.#..#
        |##.#....#.##.####...#.##
        |...########.#....#####.#
        |....#..#...##..#.#.###..
        |.####...#..#.....#......
        |#..#.##..#..###.#.##....
        |#.####..#.####.#.#.###..
        |###.#.#...#.######.#..##
        |#.####....##..########.#
        |##..##.#...#...#.#.#.#..
        |...#..#..#.#.##..###.###
        |.#.#....#.##.#...###.##.
        |###.#...#..#.##.######..
        |.#.#.###.##.##.#..#.##..
        |.####.###.#...###.#..#.#
        |..#.#..#..#.#.#.####.###
        |#..####...#.#.#.###.###.
        |#####..#####...###....##
        |#.##..#..#...#..####...#
        |.#.###..##..##..####.##.
        |...###...##...#...#..###""".stripMargin
    val tiles = parse(lines)
    val map = matchTiles(tiles)
    val rec = reconstruct(tiles, map, dim = 3)
    Tile(-1, weld(rec, dim = 3), dim = 24).flipY
      .rotateLeft(-1)
      .filledStr shouldEqual welded
  }

  "answer2" should "give filled not monster overlapping" in {
    Day20.answer2(lines) shouldEqual 273
  }
}
