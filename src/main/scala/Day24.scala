import scala.collection.mutable
import scala.io.Source

object Day24 extends App {
  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day24.txt")
    case _               => throw new IllegalArgumentException("Usage: Day24 [filename]")
  }

  case class Coord(x: Int, y: Int) {
    def walk(dir: String) = {
      val xOffset = math.abs(y % 2)
      dir match {
        case "e"  => Coord(x + 1, y)
        case "se" => Coord(x + xOffset, y + 1)
        case "sw" => Coord(x - 1 + xOffset, y + 1)
        case "w"  => Coord(x - 1, y)
        case "nw" => Coord(x - 1 + xOffset, y - 1)
        case "ne" => Coord(x + xOffset, y - 1)
      }
    }
  }

  lazy val Steps = """(se|sw|nw|ne|e|w)""".r.unanchored
  def answer(lines: Seq[String]): Int = {
    black(lines).size
  }

  private def black(lines: Seq[String]) = {
    val paths = lines.map { line =>
      Steps.findAllMatchIn(line).map(_.group(1)).toSeq
    }
    val flip =
      paths.map(path => path.foldLeft(Coord(0, 0))((acc, s) => acc.walk(s)))
    val foo = mutable.Map.empty[Coord, Int]
    val black = flip
      .foldLeft(Set.empty[Coord]) { (acc, c) =>
        (acc &~ Set(c)) | (Set(c) &~ acc)
      }
    black
  }

  lazy val directions = List("e", "se", "sw", "w", "nw", "ne")

  def check(c: Coord, s: Set[Coord]): Set[Coord] = {
    val count = directions.count(dir => s.contains(c.walk(dir)))
    if (s contains c) {
      if (count == 0 || count > 2)
        Set.empty[Coord]
      else
        Set(c)
    } else {
      if (count == 2)
        Set(c)
      else
        Set.empty[Coord]
    }

  }

  def nextState(s: Set[Coord]): Set[Coord] = {
    s.flatMap { c =>
      directions.foldLeft(check(c, s))(
        (acc, dir) => acc | check(c.walk(dir), s)
      )
    }

  }

  def answer2(lines: Seq[String], rounds: Int = 100): Int =
    Iterator
      .iterate(black(lines))(nextState)
      //.tapEach(s => println("Day X:", s.size, s))
      .drop(rounds)
      .take(1)
      .toList
      .head
      .size

  val input = source.getLines.toList
  println(input)
  println(answer(input))
  println(answer2(input))
}
