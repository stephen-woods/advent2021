import scala.io.Source

object Day1 extends App {

  val source = Source.fromFile("day1input.txt")
  val input = source.getLines().map(_.toInt).toList
  source.close()

  val input2 = List(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)

  def part1(): Int = {
    input
      .sliding(2).toList
      .foldLeft(0) { case (acc, p :: c :: Nil) =>
        if (p < c) acc + 1 else acc
      }
  }

  def part2(): Int = {
    input
      .sliding(3).toList
      .sliding(2).toList
      .foldLeft(0) { case (acc, p :: c :: Nil) =>
        if (p.sum < c.sum) acc + 1 else acc
      }
  }


  println(part1()) // 1139
  println(part2()) // 1183
}
