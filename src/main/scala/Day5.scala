import scala.io.Source

// perl version: perl -lpe 's/B|R/1/g,s/F|L/0/g,$_=oct("0b".$_)' Day5.txt | sort -n | tail -1
object Day5 {
  def seatIds(lines: List[String]): List[Int] =
    lines.map { l =>
      Integer.parseInt(
        l.replaceAll("B|R", "1")
          .replaceAll("F|L", "0"),
        2
      )
    }

  def answer(lines: List[String]): Int =
    seatIds(lines).max

  def answerFree(lines: List[String]): Option[Int] =
    seatIds(lines).sorted
      .sliding(2)
      .find {
        case List(a, b) => b - a == 2
      }
      .map(_.head + 1)

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day5.txt")
      case _               => throw new IllegalArgumentException("Usage: Day5 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answerFree(input))
  }
}
