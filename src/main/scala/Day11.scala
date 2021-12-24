import scala.annotation.tailrec
import scala.io.Source

object Day11 extends App {

  val source = Source.fromFile("day11input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin.split("\\n").toList

  val in = input

  def tick(grid: Array[Array[Int]]) : Int = {
    def flash( x: Int, y: Int): Int = {
      if (grid.indices.contains(y) && grid(y).indices.contains(x) && grid(y)(x) >= 0) {

        grid(y)(x) += 1
        if (grid(y)(x) > 9) {
          grid(y)(x) = -1
          val adjacent = (x - 1, y - 1) :: (x - 1, y) :: (x - 1, y + 1) :: (x, y + 1) :: (x + 1, y + 1) :: (x + 1, y) :: (x + 1, y - 1) :: (x, y - 1) :: Nil
          1 + adjacent.map { case (x, y) => flash(x, y) }.sum
        } else 0
      } else 0
    }

    val flashes = for {
      y <- grid.indices
      x <- grid(y).indices
    } yield flash(x, y)

    for {
      y <- grid.indices
      x <- grid(y).indices
    } if (grid(y)(x) < 0) grid(y)(x)  = 0

    flashes.sum
  }


  def renderAll(grid: Array[Array[Int]]): Unit = {
    grid.foreach{ line =>
      line.foreach{
        case 0 => print(0)
        case x => print(Console.WHITE +  x + Console.RESET)
      }
      println
    }
  }

  def allFlash(grid : Array[Array[Int]]): Boolean = {
    grid.forall(_.forall(_ == 0))
  }


  def part1(): Int = {
    val grid = in.map(_.map(_.asDigit).toArray).toArray
    println("Before any steps:")
    renderAll(grid)

    (1 to 200).foldLeft(0){ case (acc, step) =>
      val flashes = tick(grid)
      println
      println(s"After step $step:")
      renderAll(grid)
      acc + flashes
    }
  }

  def part2(): Int = {
    val grid = in.map(_.map(_.asDigit).toArray).toArray

    @tailrec
    def rec(step: Int): Int = {
      tick(grid)
      if (allFlash(grid)) step
      else rec(step + 1)
    }

    rec(1)

  }

  println(part1())
  println(part2())
}
