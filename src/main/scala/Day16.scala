import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {
  val source = Source.fromFile("day16input.txt")
  val input = source.getLines().map(_.toList).toList
  source.close()

  // Literal Packet
  val input2 = "D2FE28".toList

  // Operator length type 0
  val input3 = "38006F45291200".toList

  // Operator length type 1
  val input4 = "EE00D40C823060".toList

  val in = input2

  sealed trait Packet {
    def version: Int

    def typeId: Int
  }

  case class LiteralPacket(version: Int,
                           value: Int) extends Packet {
    val typeId = 4
  }

  case class OperatorPacket(version, Int,
                            typeId: Int,
                            lengthTypeId: Int,
                            packets: List[Packet])

  def char2bolts(c: Char): List[Boolean] = {
    val n = Character.digit(c, 16)
    val b1 = (n & 8) > 0
    val b2 = (n & 4) > 0
    val b3 = (n & 2) > 0
    val b4 = (n & 1) > 0
    b1 :: b2 :: b3 :: b4 :: Nil
  }

  def bolts2Int(bs: List[Boolean]) = {
    val zero = (0, 0)
    val (_, ret) = bs.foldRight(zero) {
      case (x, (shift, acc)) =>
        val ta = if (x) 1 << shift else 0
        (shift + 1, ta + acc)
    }
    ret
  }

  def bolts2Value(bs: List[Boolean]): Int = {
    val zero = List.empty[Boolean]

    @tailrec
    def rec(rbs: List[Boolean],
            acc : List[Boolean]): List[Boolean] = {
      rbs match {
        case true :: tail =>
          val take = tail.take(4)
          val drop = tail.drop(4)
          rec(drop, acc ::: take)

        case false :: tail =>
          val take = tail.take(4)
          acc ::: take
      }
    }
    val total = rec(bs, zero)
    println(render(total))
    bolts2Int(total)
  }




  val bolts = in.flatMap(char2bolts)


  def render(bs: List[Boolean]): String = {
    bs.map { x => if (x) "1" else "0" }.mkString
  }


  def part1(): Packet = {

    println(render(bolts))
    var bs = bolts

    val version = bolts2Int(bs.take(3))
    bs = bs.drop(3)

    val typeId = bolts2Int(bs.take(3))
    bs = bs.drop(3)

    if (typeId == 4) {
      val value = bolts2Value(bs)
      LiteralPacket(version, value)
    } else {
      val lengthTypeId = bolts2Int(bs.take(1))
      bs = bs.drop(1)



    }
  }
  println(part1())
}
