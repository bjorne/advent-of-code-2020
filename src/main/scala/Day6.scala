import scala.io.Source

// perl version: perl -lpe 's/B|R/1/g,s/F|L/0/g,$_=oct("0b".$_)' Day5.txt | sort -n | tail -1
object Day6 {
  def answer(lines: List[String]): Int =
    Day4
      .segment(lines, _.isEmpty)
      .map { group =>
        group.map(s => Set.from(s.split(""))).reduce(_ | _).size
      }
      .sum

  def answerAll(lines: List[String]): Int =
    Day4
      .segment(lines, _.isEmpty)
      .map { group =>
        group.map(s => Set.from(s.split(""))).reduce(_ & _).size
      }
      .sum

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day6.txt")
      case _               => throw new IllegalArgumentException("Usage: Day6 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answerAll(input))

  }
}
