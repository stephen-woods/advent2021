import scala.io.Source

object Day9 extends App {

  val source = Source.fromFile("day9input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin.split("\\n").toList

  val in = input

  val grid = in.map(_.map(_.asDigit).toArray).toArray


  def low(grid: Array[Array[Int]], x: Int, y: Int): Option[(Int, Int)] = {
    val depth = grid(y)(x)
    val top = if (y == 0) Int.MaxValue else grid(y - 1)(x)
    val bottom = if (y == grid.length - 1) Int.MaxValue else grid(y + 1)(x)
    val left = if (x == 0) Int.MaxValue else grid(y)(x - 1)
    val right = if (x == grid(y).length - 1) Int.MaxValue else grid(y)(x + 1)

    val surrounding = top :: bottom :: left :: right :: Nil

    if (surrounding.forall(x => x > depth)) Some((x,y)) else None
  }


  def part1(): Int = {
    val lows = for {
      y <- grid.indices
      x <- grid(y).indices
      p <- low(grid, x, y)
    } yield p

    lows
      .map{ case (x, y) => grid(y)(x) + 1}
      .sum
  }

  def part2(): Int = {
    def rec(grid: Array[Array[Int]], x: Int, y : Int): Int = {
      grid(y)(x) match {
        case 9 => 0
        case _ =>
          // Mark the grid spot
          grid(y)(x) = 9

          val top = if (y == 0) 0 else rec(grid, x, y - 1)
          val bottom = if (y == grid.length - 1) 0 else rec(grid, x, y + 1)
          val left = if (x == 0) 0 else rec(grid,x - 1, y)
          val right = if (x == grid(y).length - 1) 0 else rec(grid, x + 1, y)
          1 + top + bottom + left + right
      }
    }

    val lows = for {
      y <- grid.indices
      x <- grid(y).indices
      p <- low(grid, x, y)
    } yield p

    val sizes = lows
      .map{ case (x, y) =>
        val scratch = grid.map(_.clone())
        rec(scratch, x, y)
      }


    sizes.sorted(Ordering.Int.reverse).take(3).product
  }

  println(part1())
  println(part2())
}
