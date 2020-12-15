import scala.io.Source

object Day10 {
  def answer(lines: List[String]): Int = {
    val jolts = lines
      .map(_.toInt)
    joltDiffs(jolts).collect {
      case (diff, v) if diff == 1 || diff == 3 => v.length
    }.product
  }

  def answer2(lines: List[String]): Long = {
    val jolts = lines
      .map(_.toInt)
    val sorted = jolts.sorted
    val input = ((0 :: sorted) :+ sorted.last + 3)
    val combinations = (1 to input.length - 1).foldLeft(List(1L)) {
      (acc, index) =>
        (0 until index)
          .dropWhile(startIndex => input(startIndex) < input(index) - 3)
          .map(startIndex => acc(index - startIndex - 1))
          .sum :: acc
    }
    combinations.head
  }

  def joltDiffs(jolts: List[Int]): Map[Int, List[Int]] = {
    val sorted = jolts.sorted
    ((0 :: sorted) :+ sorted.last + 3)
      .sliding(2)
      .map {
        case List(a, b) => b - a
      }
      .toList
      .groupBy(identity)
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day10.txt")
      case _               => throw new IllegalArgumentException("Usage: Day10 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answer2(input))
  }
}
