import scala.io.Source

object Day20 extends App {
  case class Coord(x: Int, y: Int) {
    def +(that: Coord): Coord =
      Coord(this.x + that.x, this.y + that.y)

    def dim(dim: Int) = dim match {
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException(s"Invalid dim: $dim")
    }
  }

  case class Tile(id: Int, edges: List[String]) {
    override def toString = s"<Tile $id>"
  }

  def parse(lines: List[String]): List[Tile] =
    Day4.segment(lines, _.isEmpty).map {
      case header :: rest =>
        Tile(
          header.split(" ").last.dropRight(1).toInt,
          List(
            rest.map(_.head).mkString,
            rest.head,
            rest.map(_.last).mkString,
            rest.last
          )
        )
    }
  def answer(lines: List[String]): Long = {
    val tiles = parse(lines)
    val map = tiles.foldLeft(Map.empty[(Tile, Int), (Tile, Int)]) { (m, tile) =>
      tile.edges.zipWithIndex.foldLeft(m) {
        case (mm, (e, i)) if !mm.contains((tile, i)) =>
          tiles
            .filter(_ != tile)
            .collectFirst(Function.unlift { otherTile =>
              otherTile.edges.zipWithIndex.collectFirst {
                case (oe, oi) if e == oe || e == oe.reverse => (otherTile, oi)
              }
            })
            .map {
              case (ot, oi) =>
                mm.updated((tile, i), (ot, oi)).updated((ot, oi), (tile, i))
            }
            .getOrElse(mm)
        case _ => m
      }
    }
    println(map.map {
      case ((t, i), (ot, oi)) => (t.id, i) -> (ot.id, oi)
    }.toMap)
    println(map.keys.groupBy(_._1))
    val cornerTiles = map.keys
      .groupBy(_._1)
      .filter(_._2.toList.length == 2)
    println(cornerTiles.keys)
    println(cornerTiles.keys.toList.length)
    assert(cornerTiles.keys.toList.length == 4)
    cornerTiles
      .map(_._1.id.toLong)
      .product
  }

  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day20.txt")
    case _               => throw new IllegalArgumentException("Usage: Day20 [filename]")
  }

  val input = source.getLines.toList
  println(input)
  println(answer(input))
}
