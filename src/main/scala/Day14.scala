import scala.collection.mutable
import scala.io.Source

object Day14 {

  trait Instruction
  case class Mask(value: String) extends Instruction {
    lazy val zero =
      java.lang.Long.parseLong(
        value
          .replaceAll("1", "X")
          .replaceAll("0", "1")
          .replaceAll("X", "0"),
        2
      )
    lazy val one = java.lang.Long.parseLong(value.replaceAll("X", "0"), 2)
    lazy val floating = java.lang.Long
      .parseLong(value.replaceAll("1", "0").replaceAll("X", "1"), 2)
    def apply(value: Long) = (value | one) & ~zero
    def decodeAddr(addr: Long): Iterator[Long] = {
      val parts = (0 until 36)
        .map(math.pow(2L, _).toLong)
        .filter(n => 0 < (n & floating))
        .toSet
      val filtered = (addr & zero) | one
      parts.subsets.map(filtered | _.sum)
    }
  }
  case class MemSet(addr: Long, value: Long) extends Instruction

  case class Mem() {
    lazy val data: mutable.Map[Long, Long] = mutable.Map.empty
    def sum: Long = data.values.sum
  }
  case class VM(mem: Mem, mask: Mask)

  val MaskLine = raw"mask = (.+)".r
  val MemSetLine = raw"mem\[(.+)\] = (.+)".r

  def answer(lines: List[String], partOne: Boolean): Long = {
    val instructions = parseLines(lines)
    val vm = instructions
      .foldLeft(VM(Mem(), instructions.head.asInstanceOf[Mask]))((vm, i) => {
        i match {
          case Mask(_) => VM(vm.mem, i.asInstanceOf[Mask])
          case MemSet(addr, value) =>
            if (partOne)
              vm.mem
                .data(addr) = vm.mask(value)
            else
              vm.mask
                .decodeAddr(addr)
                .foreach(addr => vm.mem.data(addr) = value)
            vm
        }
      })
    vm.mem.sum
  }

  private def parseLines(lines: List[String]) = {
    lines
      .foldLeft(List.empty[List[Instruction]]) { (lists, line) =>
        line match {
          case MaskLine(mask) =>
            List(Mask(mask)) :: lists
          case MemSetLine(addr, value) =>
            (lists.head :+ MemSet(addr.toInt, value.toLong)) :: lists.tail
        }
      }
      .reverse
      .flatten
  }

  def main(args: Array[String]): Unit = {
    val source = args match {
      case Array(filename) => Source.fromFile(filename)
      case Array()         => Source.fromResource("Day14.txt")
      case _               => throw new IllegalArgumentException("Usage: Day14 [filename]")
    }
    val input = source.getLines.toList
    println(input)
    println(answer(input, true))
    println(answer(input, false))
  }
}
