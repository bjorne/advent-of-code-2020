import scala.collection.mutable
import scala.io.Source

object Day15 {
  def answerSimple(lines: List[String], n: Int = 2020): Int = {
    val input = lines.head.split(",").map(_.toInt).toList
    Iterator
      .iterate(input.reverse)(s => 1 + s.tail.indexOf(s.head) :: s)
      .drop(n - input.length)
      .take(1)
      .toList
      .head
      .head
  }

  case class State(index: Int, last: Int, lastIndex: Map[Int, Int]) {
    def next =
      if (lastIndex.contains(last))
        index - lastIndex(last) - 1
      else
        0

    def nextState = State(index + 1, next, lastIndex.updated(last, index - 1))
  }

  def answer(lines: List[String], n: Int = 2020): Int = {
    val input = lines.head.split(",").map(_.toInt).toList
    val res = Iterator
      .iterate(
        State(
          input.length,
          input.last,
          Map(input.dropRight(1).zipWithIndex: _*)
        )
      )(_.nextState)
      .drop(n - input.length)
      .take(1)
      .toList
      .head
      .last
    res
  }

  case class MutableState(var index: Int,
                          var last: Int,
                          lastIndex: mutable.Map[Int, Int]) {
    def next =
      if (lastIndex.contains(last))
        index - lastIndex(last) - 1
      else
        0

    def tick = {
      val tempLast = next
      lastIndex(last) = index - 1
      last = tempLast
      index += 1
      this
    }
  }

  def answerMutable(lines: List[String], n: Int = 2020): Int = {
    val input = lines.head.split(",").map(_.toInt).toList
    val res = Iterator
      .iterate(
        MutableState(
          input.length,
          input.last,
          mutable.Map(input.dropRight(1).zipWithIndex: _*)
        )
      )(_.tick)
      .drop(n - input.length)
      .take(1)
      .toList
      .head
      .last
    res
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day15.txt")
      case _               => throw new IllegalArgumentException("Usage: Day15 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answer(input, 30000000))
    //    println(answerMutable(input, 30000000))
  }
}
