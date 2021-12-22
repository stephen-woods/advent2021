import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day6 extends App {

  val source = Source.fromFile("day6input.txt")
  val input = source.getLines().toList.head.split(",").map(_.toInt).toList
  source.close()

  val input2 = "3,4,3,1,2".split(",").map(_.toInt).toList

  def part1(): Int = {

    def tick(in: List[Int]): List[Int] = {

      val zero = (List.empty[Int], List.empty[Int])
      val (existing, newborn) = in
        .foldRight(zero) {
          case (0, (existing, newborn)) => (6 :: existing, 8 :: newborn)
          case (x, (existing, newborn)) => (x - 1 :: existing, newborn)
        }
      existing ::: newborn
    }



    val zero = input2
    val fish = (1 to 80)
      .foldLeft(zero){ case (acc, _) => tick(acc) }

    fish.length
  }
  

  println(part1())
}
