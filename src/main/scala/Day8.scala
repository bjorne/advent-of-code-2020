import scala.io.Source

object Day8 {
  type Code = List[(String, Int)]
  sealed trait ExitCondition
  case object InfiniteLoop extends ExitCondition
  case object EndOfFile extends ExitCondition

  val Line = "(.+) (.+)".r

  def accUntilEnd(code: Code,
                  index: Int,
                  acc: Int,
                  visited: Set[Int]): (ExitCondition, Int) = {
    if (visited.contains(index)) {
      (InfiniteLoop, acc)
    } else if (index >= code.length) {
      (EndOfFile, acc)
    } else {
      val nextVisited = visited + index
      code(index) match {
        case ("nop", _) => accUntilEnd(code, index + 1, acc, nextVisited)
        case ("acc", n) => accUntilEnd(code, index + 1, acc + n, nextVisited)
        case ("jmp", n) => accUntilEnd(code, index + n, acc, nextVisited)
      }
    }
  }

  def answer(lines: List[String]): Int = {
    val code = lines.map {
      case Line(cmd, count) => (cmd, count.toInt)
    }
    val (_, acc) = accUntilEnd(code, 0, 0, Set.empty)
    acc
  }

  val mutations = Map(("jmp" -> "nop"), ("nop" -> "jmp"))
  def mutateIndex(code: Code, mutIndex: Int): Code =
    code.take(mutIndex) ++ ((mutations(code(mutIndex)._1), code(mutIndex)._2) :: code
      .takeRight(code.length - mutIndex - 1))

  def answerMutated(lines: List[String]): Int = {
    val code = lines.map {
      case Line(cmd, count) => (cmd, count.toInt)
    }
    val successIndex = (0 until code.length).find { mutIndex =>
      if (mutations.contains(code(mutIndex)._1)) {
        val mutCode = mutateIndex(code, mutIndex)
        accUntilEnd(mutCode, 0, 0, Set.empty) match {
          case (InfiniteLoop, _) => false
          case (EndOfFile, acc)  => true
        }
      } else {
        false
      }
    }
    val (_, acc) =
      accUntilEnd(mutateIndex(code, successIndex.get), 0, 0, Set.empty)
    acc
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day8.txt")
      case _               => throw new IllegalArgumentException("Usage: Day8 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answerMutated(input))
  }
}
