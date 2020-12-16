import scala.io.Source

object Day16 {
  val RuleLine = raw"(.+): (.+)".r
  val RangeStr = raw"(\d+)-(\d+)".r

  type Rule = (String, List[Range.Inclusive])
  def answer(lines: List[String]): Int = {
    val (rules, your, nearby) = parseLines(lines)
    nearby.flatMap { ticket =>
      ticket
        .filter(v => !rules.exists(_._2.exists(_.contains(v))))
    }.sum
  }

  def answer2(lines: List[String], prefix: String = "departure"): Long = {
    val (rules, your, nearby) = parseLines(lines)
    val validTickets = nearby.filter { ticket =>
      ticket
        .forall(v => rules.exists(_._2.exists(_.contains(v))))
    }
    val rulesPossibleIndexes = rules.map { rule =>
      val (_, ranges) = rule
      val indexes = (0 until validTickets.head.length).filter { fieldIndex =>
        validTickets.forall(
          ticket => ranges.exists(_.contains(ticket(fieldIndex)))
        )
      }.toList
      (rule, false, indexes)
    }
    def eliminate(
      state: Seq[(Rule, Boolean, List[Int])]
    ): Seq[(Rule, Boolean, List[Int])] =
      state
        .find {
          case (_, eliminated, indexes) => !eliminated && indexes.length == 1
        }
        .map { singleRule =>
          val (rule, _, singleIndex) = singleRule
          val newState = state.map { rpi =>
            if (rpi._1 == rule)
              (rule, true, singleIndex)
            else
              rpi match {
                case (rule, eliminated, indexes) =>
                  (rule, eliminated, indexes.filterNot(_ == singleIndex(0)))
              }
          }
          eliminate(newState)
        }
        .getOrElse(state)
    eliminate(rulesPossibleIndexes)
      .map(r => (r._1._1, r._3.head))
      .filter(_._1 startsWith prefix)
      .map(ri => your(ri._2).toLong)
      .product
  }

  private def parseLines(lines: List[String]) = {
    val segments = Day4.segment(lines, _.isEmpty)
    println(segments)
    val rules = segments(0).map {
      case RuleLine(name, values) =>
        val ranges = values.split(" or ").toList.map { range =>
          range match {
            case RangeStr(a, b) => a.toInt to b.toInt
          }
        }
        (name, ranges)
    }
    def parseTicket(s: String) = s.split(",").toList.map(_.toInt)
    val your = parseTicket(segments(1).drop(1).head)
    val nearby = segments(2).drop(1).map(parseTicket)
    (rules, your, nearby)
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day16.txt")
      case _               => throw new IllegalArgumentException("Usage: Day16 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answer2(input))
    //    println(answerMutable(input, 30000000))
  }
}
