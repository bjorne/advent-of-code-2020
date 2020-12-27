import scala.io.Source

object Day20 extends App {
  case class Coord(x: Int, y: Int) {
    def +(that: Coord): Coord =
      Coord(x + that.x, y + that.y)

    def *(len: Int): Coord =
      Coord(x * len, y * len)

    def dim(dim: Int) = dim match {
      case 0 => x
      case 1 => y
      case _ => throw new IllegalArgumentException(s"Invalid dim: $dim")
    }

    def to(o: Coord): Iterator[Coord] = o match {
      case Coord(ox, oy) if ox == x =>
        Iterator.range(y, oy + 1).map(Coord(x, _))
      case Coord(ox, oy) if oy == y =>
        Iterator.range(x, ox + 1).map(Coord(_, y))
      case _ => throw new IllegalArgumentException("Coords not in line")
    }
  }

  lazy val L = 0
  lazy val U = 1
  lazy val R = 2
  lazy val D = 3
  lazy val N = 12

  case class Tile(id: Int, filled: Set[Coord], dim: Int = 10) {
    def rotateLeft(steps: Int) =
      if ((steps + 4) % 4 == 0)
        this
      else
        Tile(id, filled.map {
          case c @ Coord(x, y) =>
            (steps + 4) % 4 match {
              case 1 => Coord(y, dim - 1 - x)
              case 2 => Coord(dim - 1 - x, dim - 1 - y)
              case 3 => Coord(dim - 1 - y, x)
            }
        }, dim)

    def flipY =
      Tile(id, filled.map {
        case Coord(x, y) => Coord(x, dim - 1 - y)
      }, dim)

    def flipX =
      Tile(id, filled.map {
        case Coord(x, y) => Coord(dim - 1 - x, y)
      }, dim)

    def flip(dir: Int) = dir match {
      case 0 => flipX
      case 1 => flipY
    }

    lazy val edges = List(
      // left
      edgeToStr(Coord(0, 0) to Coord(0, dim - 1)),
      // top
      edgeToStr(Coord(0, 0) to Coord(dim - 1, 0)),
      // right
      edgeToStr(Coord(dim - 1, 0) to Coord(dim - 1, dim - 1)),
      // bottom
      edgeToStr(Coord(0, dim - 1) to Coord(dim - 1, dim - 1))
    )

    private def edgeToStr(r: Iterator[Coord]) =
      r.map(c => if (filled.contains(c)) "#" else ".").mkString("")

    def filledStr = {
      val vs = for {
        x <- 0 until dim
        y <- 0 until dim
        c = Coord(x, y)
      } yield if (filled.contains(c)) "#" else "."
      vs.grouped(dim).map(_.mkString("")).mkString("\n")
    }

    override def toString = s"<Tile $id>"
  }

  object Tile {
    def fromSeq(id: Int, l: Seq[String], dim: Int = 10) = {
      Tile(id, l.zipWithIndex.flatMap {
        case (s, y) =>
          s.split("").zipWithIndex.collect {
            case (c, x) if c == "#" => Coord(x, y)
          }
      }.toSet, dim)
    }
  }

  def parse(lines: List[String]): List[Tile] =
    Day4.segment(lines, _.isEmpty).map {
      case header :: rest =>
        Tile.fromSeq(header.split(" ").last.dropRight(1).toInt, rest)
    }

  def answer(lines: List[String]): Long = {
    val tiles = parse(lines)
    val map = matchTiles(tiles)
    val cornerTiles = map.keys
      .groupBy(_._1)
      .filter(_._2.toList.length == 2)
    assert(cornerTiles.toList.length == 4)
    cornerTiles
      .map(_._1.id.toLong)
      .product
  }

  lazy val monsterPattern =
    """                  # 
      |#    ##    ##    ###
      | #  #  #  #  #  #   """.stripMargin.split("\n").toSeq

  def answer2(lines: List[String]): Long = {
    val tiles = parse(lines)
    val map = matchTiles(tiles)
    val dim = math.sqrt(tiles.size).toInt
    val rec = reconstruct(tiles, map, dim)
    val image = weld(rec, dim)
    val monster =
      Tile
        .fromSeq(1337, monsterPattern, monsterPattern.map(_.length).max)
        .filled
    val offsets = for {
      x <- 0 to (image.maxBy(_.x).x - monster.maxBy(_.x).x)
      y <- 0 to (image.maxBy(_.y).y - monster.maxBy(_.y).y)
    } yield {
      Coord(x, y)
    }
    val flips = List(None, Some(0), Some(1))
    val rotations = List(0, 1, 2, 3)
    val imageTile = Tile(-1, image, dim = dim * 8)
    val monsterOverlap =
      flips.collectFirst(Function.unlift { flip =>
        rotations.collectFirst(Function.unlift {
          rotation =>
            val transformedImage =
              flip
                .map(imageTile.flip(_))
                .getOrElse(imageTile)
                .rotateLeft(rotation)
                .filled
            val size = offsets
              .foldLeft(Set.empty[Coord]) { (acc, offset) =>
                val monsterOverlap = transformedImage & monster.map(_ + offset)
                if (monsterOverlap.size == monster.size) {
                  acc | monsterOverlap
                } else
                  acc
              }
              .size
            if (size > 0) {
              Some(size)
            } else {
              None
            }
        })
      })
    image.size - monsterOverlap.get
  }

  def weld(t: Map[Coord, Tile], dim: Int = N): Set[Coord] =
    coords(dim).foldLeft(Set.empty[Coord]) { (acc, c) =>
      val fillOffset = Coord(-1, -1) + c * (10 - 1 - 1)
      acc | t(c).filled.filter(coordNotEdge(_)).map(_ + fillOffset)
    }

  private def coordNotEdge(c: Coord) = {
    val notEdgeRange = (1 until (10 - 1))
    notEdgeRange.contains(c.x) && notEdgeRange.contains(c.y)
  }

  def reconstruct(tiles: List[Tile],
                  connections: Map[(Tile, Int), Set[(Tile, Int)]],
                  dim: Int = N): Map[Coord, Tile] = {
    coords(dim)
      .drop(1)
      .foldLeft(Map(Coord(0, 0) -> firstTileAligned(connections))) {
        case (prev, c @ Coord(x, y)) =>
          val (alignCoord, alignDir) = x match {
            case 0 => (c + Coord(0, -1), U)
            case _ => (c + Coord(-1, 0), L)
          }

          val alignTile = prev(alignCoord)
          val alignDirReciprocal = (alignDir + 2) % 4
          val alignTileEdge = alignTile.edges(alignDirReciprocal)
          val (tile, dir) = tiles
            .filterNot(_.id == alignTile.id)
            .collectFirst(
              Function.unlift(
                tile =>
                  tile.edges.zipWithIndex
                    .collectFirst {
                      case (edge, dir)
                          if edge == alignTileEdge || edge.reverse == alignTileEdge =>
                        (tile, dir)
                  }
              )
            )
            .get
          val rotated = tile.rotateLeft(dir - alignDir)
          val flipped =
            if (rotated.edges(alignDir) == alignTile.edges(alignDirReciprocal))
              rotated
            else
              rotated.flip((alignDir + 1) % 2)
          prev
            .updated(c, flipped)
      }
  }

  def coords(dim: Int) = {
    for {
      x <- 0 to (dim - 1)
      y <- 0 to (dim - 1)
    } yield Coord(x, y)
  }

  private def firstTileAligned(
    connections: Map[(Tile, Int), Set[(Tile, Int)]]
  ) = {
    val firstTile = connections.keys
      .groupBy(_._1)
      .find(_._2.toList.length == 2)
      .get
      ._1
    // face connecting edges down and right
    val connectingEdges =
      connections.keys.filter(_._1 == firstTile).map(_._2).toList
    assert(connectingEdges.length == 2)
    val rotation = connectingEdges(0) - R
    val rightAligned = firstTile.rotateLeft(rotation)
    val downAligned =
      if ((connectingEdges(1) - rotation + 4) % 4 != D)
        rightAligned.flipY // flip vertical
      else
        rightAligned
    downAligned
  }

  def matchTiles(tiles: List[Tile]) = {
    tiles.foldLeft(Map.empty[(Tile, Int), Set[(Tile, Int)]]) { (m, tile) =>
      tile.edges.zipWithIndex.foldLeft(m) {
        case (mm, (e, i)) =>
          tiles
            .filter(_ != tile)
            .collectFirst(Function.unlift { otherTile =>
              otherTile.edges.zipWithIndex.collectFirst {
                case (oe, oi) if e == oe || e == oe.reverse => (otherTile, oi)
              }
            })
            .map {
              case (ot, oi) =>
                val e = mm.getOrElse((tile, i), Set.empty)
                val oe = mm.getOrElse((ot, oi), Set.empty)
                mm.updated((tile, i), e + ((ot, oi)))
                  .updated((ot, oi), oe + ((tile, i)))
            }
            .getOrElse(mm)
      }
    }
  }

  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day20.txt")
    case _               => throw new IllegalArgumentException("Usage: Day20 [filename]")
  }

  val input = source.getLines.toList
  println(input)
  println(answer(input))
  println(answer2(input))
}
