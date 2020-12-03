import scala.io.Source

object Day2 {
  val passwordLine = raw"^(\d+)-(\d+) ([a-z]): (.+)".r

  def answerCount(list: List[String]): Int =
    list.count {
      case passwordLine(min, max, char, password) =>
        val count = password.count(_ == char.charAt(0))
        min.toInt <= count && max.toInt >= count
      case line => throw new IllegalArgumentException(s"Invalid line: $line")
    }

  def answerPos(list: List[String]): Int =
    list.count {
      case passwordLine(posA, posB, char, password) =>
        val isA = password.charAt(posA.toInt - 1) == char.charAt(0)
        val isB = password.charAt(posB.toInt - 1) == char.charAt(0)
        isA ^ isB
      case line => throw new IllegalArgumentException(s"Invalid line: $line")
    }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day2.txt")
      case _               => throw new IllegalArgumentException("Usage: Day2 [filename]")
    }
    val input = source.getLines.filter(!_.isEmpty).toList

    println(answerCount(input))
    println(answerPos(input))
  }
}
