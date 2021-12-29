import scala.annotation.tailrec
import scala.io.Source

object Day14 extends App {
  val source = Source.fromFile("day14input.txt")
  val input = source.getLines().toList
  source.close()

  case class Data(polymer: String, rules: Map[String, String])

  val input2 =
    """NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C""".stripMargin.split("\\n").toList

  val in = input

  val PPolymer = """^([A-Z]+)$""".r
  val PRule = """^([A-Z]+) -> ([A-Z])$""".r

  def parse(lines: List[String]): Data = {
    lines.foldLeft(Data("", Map.empty)) {
      case (acc, PPolymer(polymer)) => acc.copy(polymer = polymer)
      case (acc, PRule(x, y)) => acc.copy(rules = acc.rules + (x -> y))
      case (acc, _) => acc
    }
  }

  val data = parse(in)

  def occurrences(polymer: String): Map[Char, Long] = {
    polymer.groupMapReduce(identity)(_ => 1L)(_ + _)
  }

  def maxOccurrences(occurrences: Map[Char, Long]): Long = {
    occurrences.foldLeft(Long.MinValue) { case (acc, (_, x)) => if (x > acc) x else acc }
  }

  def minOccurrences(occurrences: Map[Char, Long]): Long = {
    occurrences.foldLeft(Long.MaxValue) { case (acc, (_, x)) => if (x < acc) x else acc }
  }


  def part1(): Long = {
    def grow(data: Data): Data = {
      @tailrec
      def rec1(polys: List[String], acc: String): String = {
        polys match {
          case Nil => acc
          case x :: Nil => rec1(Nil, acc + x)
          case x :: y => rec1(y, acc + x.dropRight(1))
        }
      }

      val segments = data
        .polymer
        .sliding(2, 1)

      val segments2 = segments
        .map { pair =>
          data.rules.get(pair) match {
            case Some(value) => pair(0) + value + pair(1)
            case None => pair
          }
        }
        .toList

      val polymer2 = rec1(segments2, "")
      Data(polymer2, data.rules)
    }

    var d1 = data
    (1 to 10).foreach{_ =>
      d1 = grow(d1)
    }

    val polymer = d1.polymer

    val occ = occurrences(polymer)

    val max = maxOccurrences(occ)
    val min = minOccurrences(occ)

    val quantity = max - min

    quantity
  }

  def part2(): Long = {
    case class PairsAndChars(pm: Map[String, Long], cm: Map[Char, Long])

    def grow(data: Data, pc: PairsAndChars): PairsAndChars = {
      pc.pm.keys.foldLeft(pc) {
        case (acc, k) => data.rules.get(k) match {
          case Some(c) =>
            val factor = pc.pm(k)
            val p1 = k(0) + c
            val p2 = c + k(1)

            val pm0 = acc.pm
            val v1 = pm0.getOrElse(p1, 0L) + factor
            val pm1 = pm0 + (p1 -> v1)
            val v2 = pm1.getOrElse(p2, 0L) + factor
            val pm2 = pm1 + (p2 -> v2)
            val v3 = pm2(k) - factor
            val pm3 = pm2 + (k -> v3)

            val cm0 = acc.cm
            val cv = cm0.getOrElse(c.head, 0L) + factor
            val cm1 = cm0 + (c.head -> cv)
            PairsAndChars(pm3, cm1)
          case None =>
            acc
        }
      }
    }

    def initial(data: Data, pairs: List[String]): PairsAndChars = {
      val pm = pairs.foldLeft(Map.empty[String, Long]) {
        case (acc, k) =>
          val v = acc.getOrElse(k, 0L) + 1
          acc + (k -> v)
      }

      val cm = data.polymer.foldLeft(Map.empty[Char, Long]) { case (acc, c) =>
        val v = acc.getOrElse(c, 0L) + 1
        acc + (c -> v)
      }
      PairsAndChars(pm, cm)
    }

    val pairs = data
      .polymer
      .sliding(2, 1)
      .toList

    var m1 = initial(data, pairs)
    (1 to 40).foreach { _ =>
      m1 = grow(data, m1)
    }


    val max = maxOccurrences(m1.cm)
    val min = minOccurrences(m1.cm)

    val quantity = max - min

    quantity
  }

  println(part1())
  println(part2())
}
