import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

class AocMath extends JavaTokenParsers {

  type I = Long

  def expr: Parser[I] = factor ~ rep(plus | times) ^^ {
    case a ~ b => (a /: b)((acc, f) => f(acc))
  }
  def plus: Parser[I => I] = "+" ~ factor ^^ { case "+" ~ b  => _ + b }
  def times: Parser[I => I] = "*" ~ factor ^^ { case "*" ~ b => _ * b }
  def factor: Parser[I] = num | "(" ~> expr <~ ")"
  def num: Parser[I] = wholeNumber ^^ (_.toInt)

}

class AocMath2 extends JavaTokenParsers {

  type I = Long

  def expr: Parser[I] = factor ~ rep(plus | times) ^^ {
    case a ~ b => (a /: b)((acc, f) => f(acc))
  }
  def times: Parser[I => I] = "*" ~ factor ^^ { case "*" ~ b => _ * b }
  def factor: Parser[I] = term ~ rep(plus) ^^ {
    case a ~ b => (a /: b)((acc, f) => f(acc))
  }
  def plus: Parser[I => I] = "+" ~ term ^^ { case "+" ~ b => _ + b }
  def term: Parser[I] = num | "(" ~> expr <~ ")"
  def num: Parser[I] = wholeNumber ^^ (_.toInt)

}

object Day18 extends App {
  def answer(lines: List[String]): Long = {
    val math = new AocMath
    lines.map(math.parseAll(math.expr, _).get).sum
  }

  def answer2(lines: List[String]): Long = {
    val math = new AocMath2
    lines.map(math.parseAll(math.expr, _).get).sum
  }

  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day18.txt")
    case _               => throw new IllegalArgumentException("Usage: Day18 [filename]")
  }

  val input = source.getLines.toList
  println(input)
  println(answer(input))
  println(answer2(input))
}
