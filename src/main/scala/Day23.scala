import scala.collection.mutable
import scala.io.Source

object Day23 extends App {
  val source = args match {
    case Array(filename) => Source.fromFile(filename)
    case Array()         => Source.fromResource("Day23.txt")
    case _               => throw new IllegalArgumentException("Usage: Day23 [filename]")
  }

  val input = source.getLines.toList
  println(input)

  type Ring = List[Int]

  def nextState(s: Ring): Ring = {
    val currentCup = s(0)
    val (before, after) = s.splitAt(1)
    val rotatedRing = after ::: before
    val (lift, rest) = rotatedRing.splitAt(3)
    val placementCup = (currentCup - 1 to currentCup - 4 by -1)
      .map(pc => (pc - 1 + s.length) % s.length + 1)
      .find(!lift.contains(_))
      .get
    rest.patch(rest.indexOf(placementCup) + 1, lift, 0)
  }

  def answer(lines: List[String], rounds: Int = 100) = {
    val input = lines(0).split("").map(_.toInt).toList
    val endState = play(input, rounds)
    val (bef, aft) = endState.splitAt(endState.indexOf(1))
    (aft ::: bef).drop(1).mkString("")
  }

  private def play(input: Ring, rounds: Int): Ring = {
    var c = 0
    val endState = Iterator
      .iterate(input)(nextState)
      .tapEach { s =>
        c += 1
        if (c % 100000 == 0) println(c)
        println(s.take(30))
      }
      .drop(rounds)
      .take(1)
      .toList
      .head
    endState
  }

  def answer2(lines: List[String], rounds: Int = 10000000): Long = {
    val input = lines(0).split("").map(_.toInt).toList
    println("000", input)
    play2(input, rounds)
  }

  def clamp(i: Int, size: Int) = (i - 1 + size) % size + 1

  type MapRing = mutable.Map[Int, Int]
  case class SparseRing(ring: mutable.Map[Int, Int], size: Int) {
    def apply(key: Int) = ring.getOrElse(key, key + 1)
    def relink(key: Int, to: Int) = {
      ring(key) = to
    }
  }
  object SparseRing {
    def fromList(input: Seq[Int], size: Int) = {
      SparseRing(
        mutable.Map(
          input
            .sliding(2)
            .map {
              case Seq(a, b) => a -> b
            }
            .toMap
            .updated(input.last, input.length + 1)
            .updated(size, input(0))
            .toSeq: _*
        ),
        size
      )
    }
  }
  type State = (Int, SparseRing)
  def nextState2(s: State): State = {
    val (currentCupLabel, ring) = s
    val liftLabels = (1 to 3)
      .foldLeft(List(currentCupLabel))((acc, _) => ring(acc.head) :: acc)
      .reverse
      .drop(1)
    val destinationCupLabel = (currentCupLabel - 1 to currentCupLabel - 4 by -1)
      .map(clamp(_, ring.size))
      .find(i => !liftLabels.contains(i))
      .get
    val destOldNext = ring(destinationCupLabel)
    ring.relink(destinationCupLabel, liftLabels.head)
    ring.relink(currentCupLabel, ring(liftLabels.last))
    ring.relink(liftLabels.last, destOldNext)
    (ring(currentCupLabel), ring)
  }

  private def play2(input: Ring, rounds: Int): Long = {
    val endState = Iterator
      .iterate((input(0), SparseRing.fromList(input, 1000000)))(nextState2)
      .drop(rounds)
      .take(1)
      .toList
      .map(_._2)
      .head
    val next = endState(1)
    val nextAfter = endState(next)
    next.toLong * nextAfter.toLong
  }

  println(answer(input))
  println(answer2(input))
}
