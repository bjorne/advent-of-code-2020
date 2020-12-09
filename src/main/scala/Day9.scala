import scala.io.Source

object Day9 {
  def answer(lines: List[String], window: Int = 25): Option[Long] = {
    val numbers = lines.map(_.toLong)
    val offendingList = numbers
      .sliding(window)
      .zip(numbers.drop(window))
      .find {
        case (list, sum) =>
          !list
            .combinations(2)
            .exists(_.sum == sum)
      }
    offendingList.map(_._2)
  }

  def answerContiguous(lines: List[String], window: Int = 25): Option[Long] = {
    val numbers = lines.map(_.toLong)
    answer(lines, window)
      .flatMap { offending =>
        (2 until numbers.length).collectFirst(
          new PartialFunction[Int, List[Long]] {
            def apply(x: Int) = windowSumsToOffending(x).get
            def isDefinedAt(x: Int) = windowSumsToOffending(x).nonEmpty
            private def windowSumsToOffending(count: Int) =
              numbers
                .sliding(count)
                .find(_.sum == offending)
          }
        )
      }
      .map(t => t.min + t.max)
  }
  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day9.txt")
      case _               => throw new IllegalArgumentException("Usage: Day9 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answerContiguous(input))

  }
}
