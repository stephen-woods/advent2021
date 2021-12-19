import scala.io.Source

object Day2 extends App {

  val source = Source.fromFile("day2input.txt")
  val input: List[(String, Int)] = source
    .getLines()
    .map { line =>
      line.split("\\s").toList match {
        case c :: u :: Nil => (c, u.toInt)
      }
    }
    .toList
  source.close()

  val input2 = List(
    ("forward", 5),
    ("down", 5),
    ("forward", 8),
    ("up", 3),
    ("down", 8),
    ("forward", 2)
  )

  def part1(): Int = {

    val (pos, depth) = input
      .foldLeft((0, 0)) {
        case ((p, d), ("forward", x)) => (p + x, d)
        case ((p, d), ("down", x)) => (p, d + x)
        case ((p, d), ("up", x)) => (p, d - x)
      }
    pos * depth
  }

  def part2(): Int = {

    val (pos, depth, aim) = input
      .foldLeft((0, 0, 0)) {
        case ((p, d, a), ("forward", x)) => (p + x, d + a * x, a)
        case ((p, d, a), ("down", x)) => (p, d, a + x)
        case ((p, d, a), ("up", x)) => (p, d, a - x)
      }
    pos * depth
  }


  println(part1())
  println(part2())
}
