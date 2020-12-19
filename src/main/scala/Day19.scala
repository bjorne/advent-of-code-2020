import scala.io.Source

object Day19 extends App {
  sealed trait Rule {
    def toR(rules: Map[Int, Rule]): String
  }
  case class Char(c: String) extends Rule {
    def toR(rules: Map[Int, Rule]) = c
  }
  case class Ref(id: Int) extends Rule {
    def toR(rules: Map[Int, Rule]) = rules(id).toR(rules)
  }
  case class SeqRule(seq: List[Ref]) extends Rule {
    def toR(rules: Map[Int, Rule]) =
      if (seq.length == 1)
        seq.head.toR(rules)
      else
        "(" + seq.map(_.toR(rules)).mkString("") + ")"
  }
  case class Or(seq: List[Rule]) extends Rule {
    def toR(rules: Map[Int, Rule]) =
      "(" + seq.map(r => r.toR(rules)).mkString("|") + ")"
  }
  case class Repeated(r: Rule) extends Rule {
    def toR(rules: Map[Int, Rule]) =
      "(" + r.toR(rules) + ")+"
  }

  def answer(lines: List[String]): Long = {
    val (segs, rules) = parse(lines)
    println("Regex is", rules(0).toR(rules))
    val r = rules(0).toR(rules).r
    segs(1).count(r.pattern.matcher(_).matches)
  }

  def answer2(lines: List[String]): Long = {
    val (segs, rules) = parse(lines)
    val newRules = rules
      .updated(8, Repeated(Ref(42))) // 8: 42 | 42 8
      .updated(
        11,
        Or(
          (0 to 5).foldLeft(List(rules(11).asInstanceOf[SeqRule]))(
            (acc, _) => SeqRule((Ref(42) :: acc.head.seq) :+ Ref(31)) :: acc
          )
        )
      ) // 11: 42 31 | 42 11 31
    val r = newRules(0).toR(newRules).r
    segs(1).count(r.pattern.matcher(_).matches)
  }

  lazy val RuleLine = raw"""(\d+): (.+)""".r
  lazy val LiteralRule = raw""""(\w)"""".r
  lazy val OrRule = raw"""(.+) \| (.+)""".r
  private def parse(lines: List[String]) = {
    val segs = Day4.segment(lines, _.isEmpty)
    val rules = segs.head.map {
      case RuleLine(idStr, rule) =>
        (idStr.toInt, rule match {
          case LiteralRule(char) => Char(char)
          case OrRule(a, b) =>
            Or(
              List(
                SeqRule(a.split(" ").map(s => Ref(s.toInt)).toList),
                SeqRule(b.split(" ").map(s => Ref(s.toInt)).toList)
              )
            )
          case str =>
            val seq = SeqRule(str.split(" ").map(s => Ref(s.toInt)).toList)
            if (seq.seq.length == 1) seq.seq.head else seq
        })
    }.toMap
    (segs, rules)
  }

  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day19.txt")
    case _               => throw new IllegalArgumentException("Usage: Day19 [filename]")
  }

  val input = source.getLines.toList
  println(input)
  println(answer(input))
  println(answer2(input))
}
