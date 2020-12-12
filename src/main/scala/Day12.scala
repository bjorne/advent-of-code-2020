import scala.io.Source

object Day12 {

  case class Coord(x: Int, y: Int) {
    def +(that: Coord): Coord = Coord(this.x + that.x, this.y + that.y)
  }
  type Command = (String, Int)

  case class State(c: Coord, dir: Int) {
    def turn(deg: Int) = copy(dir = dir + deg)
    def move(diff: Coord) = copy(c = c + diff)
    def moveForward(steps: Int) = {
      val diff = Coord(
        (steps * Math.round(Math.cos(2D * Math.PI * dir / 360D))).toInt,
        (steps * Math.round(Math.sin(2D * Math.PI * dir / 360D))).toInt
      )
      move(diff)
    }
  }

  val Line = raw"([a-zA-Z])(\d+)".r
  def parse(lines: List[String]): List[Command] =
    lines.map {
      case Line(instr, count) => (instr, count.toInt)
    }

  def answer(commands: List[Command]): Int = {
    val endState = commands.foldLeft(State(Coord(0, 0), 0)) { (state, cmd) =>
      cmd match {
        case ("L", deg)   => state.turn(deg)
        case ("R", deg)   => state.turn(-deg)
        case ("F", steps) => state.moveForward(steps)
        case ("N", steps) => state.move(Coord(0, steps))
        case ("S", steps) => state.move(Coord(0, -steps))
        case ("E", steps) => state.move(Coord(steps, 0))
        case ("W", steps) => state.move(Coord(-steps, 0))
      }
    }
    endState.c.x.abs + endState.c.y.abs
  }

  type WaypointState = (State, State)
  def rotateCoord(c: Coord, deg: Int) = {
    val turns = Math.abs(deg) / 90
    (0 until turns).foldRight(c) { (_, c) =>
      if (deg < 0) Coord(c.y, -c.x) else Coord(-c.y, c.x)
    }
  }

  def answer2(commands: List[Command]): Int = {
    val endState = commands.foldLeft((State(Coord(0, 0), 0), Coord(10, 1))) {
      (state, cmd) =>
        val (ship, wp) = state
        cmd match {
          case ("L", deg) => (ship, rotateCoord(wp, deg))
          case ("R", deg) => (ship, rotateCoord(wp, -deg))
          case ("F", steps) =>
            (ship.move(Coord(wp.x * steps, wp.y * steps)), wp)
          case ("N", steps) => (ship, wp + Coord(0, steps))
          case ("S", steps) => (ship, wp + Coord(0, -steps))
          case ("E", steps) => (ship, wp + Coord(steps, 0))
          case ("W", steps) => (ship, wp + Coord(-steps, 0))
        }
    }
    val (ship, wp) = endState
    ship.c.x.abs + ship.c.y.abs
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day12.txt")
      case _               => throw new IllegalArgumentException("Usage: Day12 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(parse(input)))
    println(answer2(parse(input)))

  }
}
