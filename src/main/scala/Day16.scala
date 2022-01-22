import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App {
  val source = Source.fromFile("day16input.txt")
  val input = source.getLines().flatMap(_.toList).toList
  source.close()

  // Literal Packet
  val input2 = "D2FE28".toList

  // Operator length type 0
  val input3 = "38006F45291200".toList

  // Operator length type 1
  val input4 = "EE00D40C823060".toList

  val input5 = "8A004A801A8002F478".toList

  val input6 = "620080001611562C8802118E34".toList

  val input7 = "C0015000016115A2E0802F182340".toList

  val input8 = "A0016C880162017C3686B18A3D4780".toList

  val in = input

  sealed trait Packet {
    def version: Int

    def typeId: Int
  }

  case class ResultTail[A](tail: List[Boolean], value: A)

  case class LiteralPacket(version: Int,
                           value: Int) extends Packet {
    val typeId = 4
  }

  case class OperatorPacket(version: Int,
                            typeId: Int,
                            lengthTypeId: Int,
                            packets: List[Packet]) extends Packet

  /**
   * Returns a list of Booleans representing four bits of binary data determined by
   * the provided hexadecimal character representation.
   *
   * @param c hexadecimal character
   */
  def char2bolts(c: Char): List[Boolean] = {
    val n = Character.digit(c, 16)
    val b1 = (n & 8) > 0
    val b2 = (n & 4) > 0
    val b3 = (n & 2) > 0
    val b4 = (n & 1) > 0
    b1 :: b2 :: b3 :: b4 :: Nil
  }

  /**
   * Returns the Integer corresponding to the the provided list of Booleans representing
   * a binary number.
   *
   * @param bs List of Booleans representing bits
   */
  def bolts2Int(bs: List[Boolean]) = {
    val zero = (0, 0)
    val (_, ret) = bs.foldRight(zero) {
      case (x, (shift, acc)) =>
        val ta = if (x) 1 << shift else 0
        (shift + 1, ta + acc)
    }
    ret
  }

  /**
   * Returns a literal packet value based on the list of Booleans representing a binary data, and
   * any remaining unused bits. Literal value packets encode a single binary number. To do this,
   * the binary number is padded with leading zeroes until its length is a multiple of four bits,
   * and then it is broken into groups of four bits. Each group is prefixed by a 1 bit except the
   * last group, which is prefixed by a 0 bit.
   *
   * @param bs List of Booleans representing bits
   */
  def bolts2Value(bs: List[Boolean]): ResultTail[Int] = {
    val zero = List.empty[Boolean]

    @tailrec
    def rec(rbs: List[Boolean],
            acc: List[Boolean]): ResultTail[List[Boolean]] = {
      rbs match {
        case true :: tail =>
          val (take, drop) = tail.splitAt(4)
          rec(drop, acc ::: take)

        case false :: tail =>
          val (take, drop) = tail.splitAt(4)
          ResultTail(drop, acc ::: take)
      }
    }

    val r = rec(bs, zero)
    r.copy(value = bolts2Int(r.value))
  }




  def parse(bits: List[Boolean]): ResultTail[Packet] = {

    @tailrec
    def parse0(bs: List[Boolean],
               acc : List[Packet]): List[Packet] = {
      bs match {
        case Nil => acc.reverse
        case _ =>
          val r = parse(bs)
          parse0(r.tail, r.value :: acc)
      }
    }

    @tailrec
    def parse1(bs: List[Boolean],
               acc : List[Packet],
               count: Int): ResultTail[List[Packet]] = {
      if (count == 0)
        ResultTail(bs, acc.reverse)
      else {
        val r = parse(bs)
        parse1(r.tail, r.value :: acc, count - 1)
      }
    }

    // Every packet begins with a standard header: the first three bits encode the packet version
    val (versionBits, versionTail) = bits.splitAt(3)
    val version = bolts2Int(versionBits)

    // The next three bits encode the packet type ID
    val (typeIdBits, typeIdTail) = versionTail.splitAt(3)
    val typeId = bolts2Int(typeIdBits)

    if (typeId == 4) {
      // Packets with type ID 4 represent a literal value. Literal value packets encode a single
      // binary number.
      val r = bolts2Value(typeIdTail)
      ResultTail(r.tail, LiteralPacket(version, r.value))
    } else {
      // Every other type of packet (any packet with a type ID other than 4) represent an operator
      // that performs some calculation on one or more sub-packets contained within.
      val (lengthTypeIdBits, lengthTypeIdTail) = typeIdTail.splitAt(1)
      val lengthTypeId = bolts2Int(lengthTypeIdBits)
      if (lengthTypeId == 0) {
        // If the length type ID is 0, then the next 15 bits are a number that represents
        // the total length in bits of the sub-packets contained by this packet.
        val (subLengthBits, subLengthTail) = lengthTypeIdTail.splitAt(15)
        val subLength = bolts2Int(subLengthBits)
        val (subBits, subBitTail) = subLengthTail.splitAt(subLength)
        val packets = parse0(subBits, Nil)
        ResultTail(subBitTail, OperatorPacket(version, typeId, lengthTypeId, packets))

      } else {
        // If the length type ID is 1, then the next 11 bits are a number that represents
        // the number of sub-packets immediately contained by this packet
        val (subLengthBits, subLengthTail) = lengthTypeIdTail.splitAt(11)
        val subLength = bolts2Int(subLengthBits)
        val r =parse1(subLengthTail, Nil, subLength)
        val packets = r.value
        ResultTail(r.tail, OperatorPacket(version, typeId, lengthTypeId, packets))
      }
    }
  }

  val bolts = in.flatMap(char2bolts)


  def render(bs: List[Boolean]): String = {
    bs.map { x => if (x) "1" else "0" }.mkString
  }




  def part1(): Int = {
    def sumVersions(packet: Packet): Int = {
      packet match {
        case LiteralPacket(version, _) => version
        case OperatorPacket(version, _, _, packets) =>
          packets.map(sumVersions).sum + version
      }
    }

    println(render(bolts))
    val packet = parse(bolts).value

    sumVersions(packet)
  }

  println(part1())
}
