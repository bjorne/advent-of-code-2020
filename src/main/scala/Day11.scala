import scala.collection.immutable.ArraySeq
import scala.io.Source

object Day11 {
  case class Coord(row: Int, col: Int) {
    def +(that: Coord): Coord = Coord(this.row + that.row, this.col + that.col)
  }
  case class Grid[A](data: List[List[A]]) {
    def apply(c: Coord): Option[A] =
      data.lift(c.row).flatMap(_.lift(c.col))

    def mapWithCoord(f: (A, Coord) => A): Grid[A] =
      Grid(data.zipWithIndex.map {
        case (row, r) =>
          row.zipWithIndex.map {
            case (v, c) => f(v, Coord(r, c))
          }
      })
  }
  type State = Grid[String]
  def parse(lines: List[String]): State =
    Grid(lines.map(_.split("").toList))

  def isOccupied(state: State)(c: Coord): Boolean =
    state(c).contains("#")

  def collectNeighborhood[A](c: Coord,
                             fn: Coord => A,
                             radius: Int = 1): Seq[A] =
    for {
      rd <- -radius until (radius + 1)
      cd <- -radius until (radius + 1)
      if rd != 0 || cd != 0
      offset = Coord(rd, cd)
    } yield fn(c + offset)

  def countNeighbors(state: State, c: Coord) =
    collectNeighborhood(c, isOccupied(state)).count(identity)

  def nextState(state: State): State = {
    state.mapWithCoord {
      case (".", _) => "."
      case (v, c) =>
        val adj = countNeighbors(state, c)
        v match {
          case "L" if adj == 0 => "#"
          case "#" if adj >= 4 => "L"
          case x               => x
        }
    }
  }
  def answer(state: State): Int = {
    val endState = Iterator
      .iterate(state)(nextState)
      .sliding(2)
      .takeWhile {
        case ArraySeq(s1, s2) => s1 != s2
      }
      .map(_.last)
      .reduce((_, b) => b)
    collectNeighborhood(
      Coord(-1, -1),
      isOccupied(endState),
      endState.data.length + 1
    ).count(identity)
  }

  def findSeat(state: State, c: Coord, dir: Coord): Option[Coord] = {
    val next = c + dir
    state(next) match {
      case Some("#") => Some(next)
      case Some("L") => Some(next)
      case Some(".") => findSeat(state, next, dir)
      case None      => None
    }
  }

  def countNeighborDirs(state: State, c: Coord) =
    collectNeighborhood(
      Coord(0, 0),
      dir => findSeat(state, c, dir).flatMap(state(_)).contains("#")
    ).count(identity)

  def nextState2(state: State): State = {
    state.mapWithCoord {
      case (".", _) => "."
      case (v, c) =>
        val adj = countNeighborDirs(state, c)
        v match {
          case "L" if adj == 0 => "#"
          case "#" if adj >= 5 => "L"
          case x               => x
        }
    }
  }

  def answer2(state: State): Int = {
    val endState = Iterator
      .iterate(state)(nextState2)
      .sliding(2)
      .takeWhile {
        case ArraySeq(s1, s2) => s1 != s2
      }
      .map(_.last)
      .reduce((_, b) => b)
    collectNeighborhood(
      Coord(-1, -1),
      isOccupied(endState),
      endState.data.length + 1
    ).count(identity)
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day11.txt")
      case _               => throw new IllegalArgumentException("Usage: Day11 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(parse(input)))
    println(answer2(parse(input)))

  }
}
