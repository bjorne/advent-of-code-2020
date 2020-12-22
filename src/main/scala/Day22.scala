import scala.io.Source

object Day22 extends App {
  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day22.txt")
    case _               => throw new IllegalArgumentException("Usage: Day22 [filename]")
  }

  val input = source.getLines.toList
  println(input)

  case class Hand(player: Int, cards: List[Int]) {
    def take(n: Int): Hand = copy(cards = cards.take(n))

    def pop: (Int, Hand) = cards match {
      case h :: t => (h, copy(cards = t))
    }

    def unshift(card: Int) = copy(cards = cards :+ card)

    def notEmpty = cards.length > 0

    def score =
      cards.zip(cards.length to 1 by -1).map(z => z._1 * z._2).sum

    def size = cards.length

    def isEmpty = !notEmpty

  }

  def nextHands(hands: (Hand, Hand), recursive: Boolean): (Hand, Hand) = {
    val (p1c, p1n) = hands._1.pop
    val (p2c, p2n) = hands._2.pop
    val p1wins = if (recursive && p1c <= p1n.size && p2c <= p2n.size) {
      1 == play((p1n.take(p1c), p2n.take(p2c)), recursive).player
    } else {
      p1c > p2c
    }
    if (p1wins)
      (p1n.unshift(p1c).unshift(p2c), p2n)
    else
      (p1n, p2n.unshift(p2c).unshift(p1c))
  }

  private def play(hands: (Hand, Hand), recursive: Boolean = false) = {
    Iterator
      .iterate((Set.empty[(Hand, Hand)], hands)) {
        case (seenHands, lastHands) =>
          (seenHands + lastHands, nextHands(lastHands, recursive))
      }
      .collectFirst {
        case (seen, hands)
            if seen.contains(hands) || hands._1.isEmpty || hands._2.isEmpty =>
          hands match {
            case (p1, _) if p1.notEmpty => p1
            case (_, p2)                => p2
          }
      }
      .get
  }

  def answer(lines: List[String], recursive: Boolean): Int = {
    val cards = Day4.segment(lines, _.isEmpty).map(_.tail.map(_.toInt)) match {
      case List(p1, p2) => (Hand(1, p1), Hand(2, p2))
    }
    val winner = play(cards, recursive)
    winner.score
  }

  println(answer(input, false))

  println(answer(input, true))

}
