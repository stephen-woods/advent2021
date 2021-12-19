import java.lang.Integer.parseInt
import scala.annotation.tailrec
import scala.collection.BitSet
import scala.io.Source

object Day3 extends App {
  val source = Source.fromFile("day3input.txt")
  val input: List[String] = source.getLines().toList
  source.close()

  val input2: List[String] =
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010"""
      .stripMargin
      .split("\\n")
      .toList


  def bitO2Criteria(bits: List[Int]) = {
    if (bits.count(_ == 1) >= bits.count(_ == 0)) 1 else 0
  }

  def bitCO2Criteria(bits: List[Int]) = {
    if (bits.count(_ == 1) >= bits.count(_ == 0)) 0 else 1
  }


  def part1(): Int = {

    def rec(numbers: List[String],
            fn: (Int, Int) => Boolean): String = {
      if (numbers.head.isEmpty) ""
      else {
        val sum = numbers.map(_.head.asDigit).sum
        val digit = if (fn(sum, numbers.length / 2)) "1" else "0"
        digit + rec(numbers.map(_.tail), fn)
      }
    }

    val gamma = parseInt(rec(input, _ > _), 2)
    val epsilon = parseInt(rec(input, _ < _), 2)

    gamma * epsilon
  }

  def part1b(): Int = {
    val cs = input.toArray.map(_.toArray)
    val tr = cs.transpose

    val sGamma = tr
      .map { xs =>
        if (xs.count(_ == '0') > xs.length / 2) '0' else '1'
      }
      .mkString("")

    val sEpsilon = tr
      .map { xs =>
        if (xs.count(_ == '0') < xs.length / 2) '0' else '1'
      }
      .mkString("")

    val gamma = parseInt(sGamma, 2)
    val epsilon = parseInt(sEpsilon, 2)
    gamma * epsilon
  }

  def part2(): Int = {
    val cs = input.map(_.map(_.asDigit).toList)
    val tr = cs.transpose

    @tailrec
    def rec(tr: List[List[Int]],
            cs: List[List[Int]],
            criteria: List[Int] => Int,
            i: Int): List[Int] = {
      val crit = criteria(tr(i))
      val cs2: List[List[Int]] = cs.filter(x => x(i) == crit)
      val tr2 = cs2.transpose
      cs2 match {
        case x :: Nil => x
        case _ => rec(tr2, cs2, criteria, i + 1)
      }
    }

    val so2 = rec(tr, cs, bitO2Criteria, 0).mkString
    val sco2 = rec(tr, cs, bitCO2Criteria, 0).mkString
    val oxygen = parseInt(so2, 2)
    val co2 = parseInt(sco2, 2)
    oxygen * co2
  }

  println(part1())
  println(part2())
}
