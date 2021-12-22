import scala.io.Source

object Day8 extends App {
  val source = Source.fromFile("day8input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
      |""".stripMargin.split("\\n").toList

  val input3 = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf" :: Nil

  val in = input
  val P = """^(\w+)\s(\w+)\s(\w+)\s(\w+)\s(\w+)\s(\w+)\s(\w+)\s(\w+)\s(\w+)\s(\w+)\s\|\s(\w+)\s(\w+)\s(\w+)\s(\w+).*$""".r

  def part1(): Int = {
    val uniqueLengths = List(2, 3, 4, 7)

    val P1 = """^.*\|\s(\w+)\s(\w+)\s(\w+)\s(\w+).*$""".r
    in.map {
      case P1(d1, d2, d3, d4) =>
        val digits = d1 :: d2 :: d3 :: d4 :: Nil

        digits.count { x =>
          uniqueLengths.contains(x.length)
        }
    }.sum
  }

  def part2(): Int = {

    val digs = input.map {
      case P(p1, p2, p3, p4, p5, p6, p7, p8, p9, p0, e1, e2, e3, e4) =>

        // By deduction, determine which pattern is associated with each digit 1-0
        val all = Set(p1, p2, p3, p4, p5, p6, p7, p8, p9, p0)

        val d1 = all.filter(_.length == 2).head
        val d4 = all.filter(_.length == 4).head
        val d7 = all.filter(_.length == 3).head
        val d8 = all.filter(_.length == 7).head

        val d9 = {
          all
            .filter(_.length == 6)
            .filter(x => d4.toSet.forall(x.toSet.contains))
            .head
        }

        val d6 = {
          val start = all.filter(_.length == 6) - d9
          val fives = all.filter(_.length == 5)
          start
            .filter(x => fives.exists(y => y.toSet.forall(x.toSet.contains)))
            .head
        }

        val d0 = (all.filter(_.length == 6) - d9 - d6).head

        val d5 = {
          all
            .filter(_.length == 5)
            .filter(x => x.toSet.forall(d6.toSet.contains))
            .head
        }

        val d3 = {
          val start = all.filter(_.length == 5) - d5
          start
            .filter(x => d1.toSet.forall(x.toSet.contains))
            .head
        }

        val d2 = (all.filter(_.length == 5) - d5 - d3).head

        // Create a translation of pattern character sets to digits
        val trs = List((d1.toSet, 1), (d2.toSet, 2), (d3.toSet, 3), (d4.toSet, 4), (d5.toSet, 5), (d6.toSet, 6), (d7.toSet, 7), (d8.toSet, 8), (d9.toSet, 9), (d0.toSet, 0))

        //        println(s"$d1 = 1")
        //        println(s"$d2 = 2")
        //        println(s"$d3 = 3")
        //        println(s"$d4 = 4")
        //        println(s"$d5 = 5")
        //        println(s"$d6 = 6")
        //        println(s"$d7 = 7")
        //        println(s"$d8 = 8")
        //        println(s"$d9 = 9")
        //        println(s"$d0 = 0")

        def decode(trs: List[(Set[Char], Int)],
                   e: String): Int = {
          val Target = e.toSet
          trs.find {
            case (Target, _) => true
            case _ => false
          }
            .map(_._2)
            .get
        }

        val output = decode(trs, e1) * 1000 +
          decode(trs, e2) * 100 +
          decode(trs, e3) * 10 +
          decode(trs, e4)
        output
    }
    digs.sum
  }

  println(part1())
  println(part2())
}
