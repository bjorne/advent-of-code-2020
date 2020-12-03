import scala.io.Source

object Day3 {
  def answer(lines: List[String], right: Int, down: Int): Int =
    lines.zipWithIndex
      .count {
        case (line, index) =>
          index % down == 0 && line.charAt(index / down * right % line.length) == '#'
      }

  def answerProduct(lines: List[String], slopes: List[(Int, Int)]): Int =
    slopes.map {
      case (right, down) => answer(lines, right, down)
    }.product

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day3.txt")
      case _               => throw new IllegalArgumentException("Usage: Day3 [filename]")
    }
    val input = source.getLines.filter(!_.isEmpty).toList

    println(answer(input, 3, 1))
    println(answerProduct(input, List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))))
  }
}
