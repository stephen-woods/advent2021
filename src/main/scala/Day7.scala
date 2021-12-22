import scala.collection.mutable
import scala.io.Source

object Day7 extends App {

  val source = Source.fromFile("day7input.txt")
  val input = source.getLines().toList.head.split(",").map(_.toInt).toList
  source.close()

  val input2 = "16,1,2,0,4,2,7,1,2,14".split(",").map(_.toInt).toList

  val in = input
  val min = in.min
  val max = in.max
  val mm = mutable.Map.empty[Int, Int]

  def part1(): Int = {


    (min to max).foreach { p =>
      //      println(s"Position $p")
      val fuel = in
        .map { x =>
          val f = Math.abs(p - x)
          //          println(s" - Move from $x to $p: $f fuel")
          f
        }
        .sum
      //      println(s" Total fuel: $fuel")
      //      println
      mm.put(p, fuel)
    }

    val (pos, fuel) = mm.min((x: (Int, Int), y: (Int, Int)) => x._2.compare(y._2))
    fuel
  }


  def part2(): Int = {

    (min to max).foreach { p =>
      //      println(s"Position $p")
      val fuel = in
        .map { x =>
          val d = Math.abs(p - x)
          val f = (0 to d).sum
          //          println(s" - Move from $x to $p: $f fuel")
          f
        }
        .sum
      //      println(s" Total fuel: $fuel")
      //      println
      mm.put(p, fuel)
    }

    val (pos, fuel) = mm.min((x: (Int, Int), y: (Int, Int)) => x._2.compare(y._2))
    fuel
  }

  println(part1())
  println(part2())

}
