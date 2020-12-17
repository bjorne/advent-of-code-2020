import scala.io.Source

object Day17 {
  case class Coord(x: Int, y: Int, z: Int) {
    def +(that: Coord): Coord =
      Coord(this.x + that.x, this.y + that.y, this.z + that.z)

    def dim(dim: Int) = dim match {
      case 0 => x
      case 1 => y
      case 2 => z
      case _ => throw new IllegalArgumentException(s"Invalid dim: $dim")
    }

    def collectNeighborhood[A](fn: Coord => A, radius: Int = 1): Seq[A] =
      for {
        xd <- -radius until (radius + 1)
        yd <- -radius until (radius + 1)
        zd <- -radius until (radius + 1)
        if xd != 0 || yd != 0 || zd != 0
        offset = Coord(xd, yd, zd)
      } yield fn(this + offset)

  }
  case class Grid[A](data: Map[Coord, A]) {
    def minDim(dim: Int): Int = data.keys.map(_.dim(dim)).min

    def maxDim(dim: Int): Int = data.keys.map(_.dim(dim)).max

    def apply(c: Coord): Option[A] =
      data get c

    def set(c: Coord, v: A): Grid[A] =
      Grid(data.updated(c, v))

    override def toString =
      (minDim(2) to maxDim(2))
        .map { z =>
          s"z=$z\n" + (minDim(0) to maxDim(0))
            .map { x =>
              (minDim(1) to maxDim(1)).map { y =>
                apply(Coord(x, y, z)).getOrElse(" ")
              }.mkString
            }
            .mkString("\n")
        }
        .mkString("\n\n")
  }
  type State = Grid[String]
  def parse(lines: List[String]): State =
    Grid(lines.zipWithIndex.flatMap {
      case (l, xi) =>
        l.split("").zipWithIndex.map {
          case (v, yi) => (Coord(xi, yi, 0), v)
        }
    }.toMap)

  def isOccupied(state: State)(c: Coord): Boolean =
    state(c).contains("#")

  def countNeighbors(state: State, c: Coord) =
    c.collectNeighborhood(isOccupied(state)).count(identity)

  def nextState(state: State): State = {
    val newData = ((state.minDim(0) - 1) to (state.maxDim(0) + 1)).flatMap {
      x =>
        ((state.minDim(1) - 1) to (state.maxDim(1) + 1)).flatMap { y =>
          ((state.minDim(2) - 1) to (state.maxDim(2) + 1)).map { z =>
            val c = Coord(x, y, z)
            val turf = countNeighbors(state, c)
            (c, state(c) match {
              case _ if turf == 3         => "#"
              case Some("#") if turf == 2 => "#"
              case x                      => "."
            })
          }
        }
    }.toMap
    val s = Grid(newData)
    println(s.toString)
    s
  }

  def answer(lines: List[String]): Int = {
    val state = parse(lines)
    println(state.toString)
    val endState =
      Iterator.iterate(state)(nextState).drop(6).take(1).toList.head
    endState.data.values.count(_ == "#")
  }

  case class Coord4(x: Int, y: Int, z: Int, w: Int) {
    def +(that: Coord4): Coord4 =
      Coord4(this.x + that.x, this.y + that.y, this.z + that.z, this.w + that.w)

    def dim(dim: Int) = dim match {
      case 0 => x
      case 1 => y
      case 2 => z
      case 3 => w
      case _ => throw new IllegalArgumentException(s"Invalid dim: $dim")
    }

    def collectNeighborhood[A](fn: Coord4 => A, radius: Int = 1): Seq[A] =
      for {
        xd <- -radius until (radius + 1)
        yd <- -radius until (radius + 1)
        zd <- -radius until (radius + 1)
        wd <- -radius until (radius + 1)
        if xd != 0 || yd != 0 || zd != 0 || wd != 0
        offset = Coord4(xd, yd, zd, wd)
      } yield fn(this + offset)

  }
  case class Grid4[A](data: Map[Coord4, A]) {
    def minDim(dim: Int): Int = data.keys.map(_.dim(dim)).min

    def maxDim(dim: Int): Int = data.keys.map(_.dim(dim)).max

    def apply(c: Coord4): Option[A] =
      data get c

    def set(c: Coord4, v: A): Grid4[A] =
      Grid4(data.updated(c, v))
  }
  type State4 = Grid4[String]
  def parse4(lines: List[String]): State4 =
    Grid4(lines.zipWithIndex.flatMap {
      case (l, xi) =>
        l.split("").zipWithIndex.map {
          case (v, yi) => (Coord4(xi, yi, 0, 0), v)
        }
    }.toMap)

  def isOccupied4(state: State4)(c: Coord4): Boolean =
    state(c).contains("#")

  def countNeighbors4(state: State4, c: Coord4) =
    c.collectNeighborhood(isOccupied4(state)).count(identity)

  def nextState4(state: State4): State4 = {
    val newData = ((state.minDim(0) - 1) to (state.maxDim(0) + 1)).flatMap {
      x =>
        ((state.minDim(1) - 1) to (state.maxDim(1) + 1)).flatMap { y =>
          ((state.minDim(2) - 1) to (state.maxDim(2) + 1)).flatMap { z =>
            ((state.minDim(3) - 1) to (state.maxDim(3) + 1)).map { w =>
              val c = Coord4(x, y, z, w)
              val turf = countNeighbors4(state, c)
              (c, state(c) match {
                case _ if turf == 3         => "#"
                case Some("#") if turf == 2 => "#"
                case x                      => "."
              })
            }
          }
        }
    }.toMap
    val s = Grid4(newData)
    s
  }

  def answer4(lines: List[String]): Int = {
    val state = parse4(lines)
    val endState =
      Iterator.iterate(state)(nextState4).drop(6).take(1).toList.head
    endState.data.values.count(_ == "#")
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day17.txt")
      case _               => throw new IllegalArgumentException("Usage: Day17 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answer4(input))
  }
}
