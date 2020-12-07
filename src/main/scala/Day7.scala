import scala.io.Source

object Day7 {
  val Line = "(.+) bags contain (.+)".r
  val EmptyBag = "no other".r.unanchored
  val CountOfColor = raw"(\d+) (.+) bag".r.unanchored

  private def extractTree(lines: List[String]) = {
    lines.map {
      case Line(subject, contain) =>
        subject -> (contain match {
          case EmptyBag() => Map.empty[String, Int]
          case _ =>
            contain
              .split(",")
              .map {
                case CountOfColor(count, color) =>
                  color -> count.toInt
                case record =>
                  throw new IllegalArgumentException(
                    s"Invalid record: '$record'"
                  )
              }
              .toMap
        })
    }.toMap
  }

  def answer(lines: List[String]): Int = {
    val map = extractTree(lines)
    def rootsToBag(roots: List[String],
                   map: Map[String, Map[String, Int]],
                   needle: String): Int =
      roots.count { root =>
        (map(root) get needle match {
          case Some(count) => count > 0
          case None        => false
        }) || rootsToBag(map(root).keys.toList, map, needle) > 0
      }
    rootsToBag(map.keys.toList, map, "shiny gold")
  }

  def answerSum(lines: List[String]): Int = {
    val map = extractTree(lines)
    def size(bag: String, map: Map[String, Map[String, Int]]): Int =
      map(bag).map {
        case (k, count) => {
          count * (map(k).size match {
            case 0 => 1
            case _ => 1 + size(k, map)
          })
        }
      }.sum
    size("shiny gold", map)
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day7.txt")
      case _               => throw new IllegalArgumentException("Usage: Day7 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answerSum(input))

  }
}
