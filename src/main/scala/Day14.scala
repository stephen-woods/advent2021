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

  def tick1(data: Data): Data = {
    @tailrec
    def rec1(polys: List[String], acc :String): String = {
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

  def occurrences(polymer: String): Map[Char, Long] = {
    polymer.groupMapReduce(identity)(_ => 1L)(_ + _)
  }

  def maxOccurrences(occurrences: Map[Char, Long]): Long = {
    occurrences.foldLeft(Long.MinValue){ case (acc, (_,x)) => if (x > acc) x else acc }
  }

  def minOccurrences(occurrences: Map[Char, Long]): Long = {
    occurrences.foldLeft(Long.MaxValue){ case (acc, (_,x)) => if (x < acc) x else acc }
  }

  @tailrec
  def grow(data: Data, steps: Int): Data = {
    steps match {
      case 0 => data
      case x => grow(tick1(data), x - 1)
    }
  }


  def part1(): Long = {
    val polymer = grow(data, 10).polymer

    val occ = occurrences(polymer)

    val max = maxOccurrences(occ)
    val min = minOccurrences(occ)

    val quantity = max - min

    quantity
  }

  println(part1())
}
