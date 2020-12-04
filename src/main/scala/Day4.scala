import scala.collection.Map
import scala.io.Source

object Day4 {
  def segment(lines: List[String], p: String => Boolean): List[List[String]] = {
    lines.foldRight(List.empty[List[String]]) {
      (str: String, acc: List[List[String]]) =>
        str match {
          case "" => List() :: acc
          case nonEmpty =>
            acc match {
              case Nil    => List(nonEmpty) :: acc
              case h :: t => (nonEmpty :: h) :: t
            }
        }
    }
  }

  def parseLines(lines: List[String]): List[Map[String, String]] =
    segment(lines, _.isEmpty)
      .map(
        _.flatMap(_.split(" "))
          .map(_.split(":"))
          .map {
            case Array(a, b) => a -> b
            case s =>
              throw new IllegalArgumentException(s"Invalid input line: $s")
          }
          .toMap
      )

  val hgtRegex = "\\A(\\d+)(cm|in)\\Z".r
  val hclRegex = "\\A#[a-f0-9]{6}\\Z".r
  val eclRegex = "\\A(amb|blu|brn|gry|grn|hzl|oth)\\Z".r
  val pidRegex = "\\A\\d{9}\\Z".r

  val fieldValidations: Map[String, String => Boolean] = Map(
    "byr" -> (n => n.toInt >= 1920 && n.toInt <= 2002),
    "iyr" -> (n => n.toInt >= 2010 && n.toInt <= 2020),
    "eyr" -> (n => n.toInt >= 2020 && n.toInt <= 2030),
    "hgt" -> {
      case hgtRegex(num, unit) =>
        unit match {
          case "cm" => num.toInt >= 150 && num.toInt <= 193
          case "in" => num.toInt >= 59 && num.toInt <= 76
          case _    => false
        }
      case _ => false
    },
    "hcl" -> hclRegex.matches,
    "ecl" -> eclRegex.matches,
    "pid" -> pidRegex.matches
  )
  val requiredFields = fieldValidations.keys

  def answer(lines: List[String]): Int =
    parseLines(lines)
      .map(_.keys.toList)
      .count(list => requiredFields.forall(list.contains(_)))

  def answerValidated(lines: List[String]): Int =
    parseLines(lines)
      .count(
        values =>
          fieldValidations.forall {
            case key -> p => (values get key).exists(p(_))
        }
      )

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day4.txt")
      case _               => throw new IllegalArgumentException("Usage: Day4 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input))
    println(answerValidated(input))

  }
}
