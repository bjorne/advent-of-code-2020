import scala.io.Source

object Day1 {
  def answer(list: Seq[Int], count: Int): Option[Int] =
    list
      .combinations(count)
      .find(_.sum == 2020)
      .map(_.product)

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day1.txt")
      case _               => throw new IllegalArgumentException("Usage: Day1 [filename]")
    }
    val input = source.getLines.filter(!_.isEmpty).map(_.toInt).toSeq

    println(answer(input, 2))
    println(answer(input, 3))
//    print(answer(input))
  }
}
