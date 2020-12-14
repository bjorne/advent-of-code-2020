import scala.io.Source

object Day13 {

  def answer(lines: List[String]): Int = {
    val start = lines.head.toInt
    val buses = lines.last.split(",").filter(_ != "x").map(_.toInt)
    val nextBus = buses.minBy(bus => (start / bus + 1) * bus)
    nextBus * ((start / nextBus + 1) * nextBus - start)
  }

  // minimize x0 such that:
  //  x0 = x1 * 7
  //  x0 = -1 + x2 * 13
  //  x0 = -4 + x3 * 59
  //  x0 = -6 + x4 * 31
  //  x0 = -7 + x5 * 19

  // x1 = t / 7
  // x2 = (t + 1) / 13

  // 7,x,3,5
  // 7 => 1x7; 2x3+2 - 1; 1x5+3 - 1
  // 14 => 2x7; 4x3+2; 3x5+3 - 4
  // 21 => 3x7; 7x3+2 - 2; 4*5+3 - 2
  // 28 => 4x7; 9x3+2 - 1; 5x5+3
  // 35 => 5x7;
  type Bus = (Int, Int)
  def answer2(lines: List[String]): Long = {
    val buses = lines.last
      .split(",")
      .zipWithIndex
      .filter(_._1 != "x")
      .map(x => (x._1.toInt, x._2))
    buses
      .scanLeft(List.empty[Bus])((l, b) => l :+ b)
      .drop(1)
      .foldLeft(buses.head._2.toLong) { (x, cumBuses) =>
        val step = cumBuses.dropRight(1).map(_._1.toLong).product
        Iterator
          .iterate(x + step)(_ + step)
          .find { x =>
            val (nl, al) = cumBuses.last
            (x + al) % nl == 0
          }
          .get
      }
  }

  def answer2Brute(lines: List[String]): Option[Int] = {
    val buses = lines.last
      .split(",")
      .zipWithIndex
      .filter(_._1 != "x")
      .map(x => (x._1.toInt, x._2))
    Iterator
      .iterate(0)(_ + buses.head._1)
      .find { x =>
        buses.tail.forall {
          case (bus, index) => (x + index) % bus == 0
        }
      }
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day13.txt")
      case _               => throw new IllegalArgumentException("Usage: Day13 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answer2(input))
  }
}
