import scala.io.Source

object Day25 extends App {
  lazy val subjectNumber = 7L
  lazy val divisor = 20201227L

  def answer(lines: Seq[String]): Long = {
    val cardKey = lines(0).toLong
    val doorKey = lines(1).toLong
    val cardLoopSize = loop(subjectNumber)
      .find(_._2 == cardKey)
      .map(_._1)
      .get
//    val doorLoopSize = loop(subjectNumber)
//      .find(_._2 == doorKey)
//      .map(_._1)
//      .get
    val encryptionKey = loop(doorKey)
      .find(_._1 == cardLoopSize)
      .map(_._2)
      .get
    encryptionKey
  }

  private def loop(subjectNumber: Long) = {
    Iterator
      .iterate((0, 1L)) {
        case (i, v) => (i + 1, (v * subjectNumber) % divisor)
      }
  }

  lazy val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day25.txt")
    case _               => throw new IllegalArgumentException("Usage: Day25 [filename]")
  }
  lazy val input = source.getLines.toList
  println(input)
  println(answer(input))
}
