import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {

  val source = Source.fromFile("day13input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5
      |""".stripMargin.split("\\n").toList


  val in = input

  case class Coord(x: Int, y: Int)

  sealed trait FoldAlong

  case class FoldVerticalX(x: Int) extends FoldAlong

  case class FoldHorizontalY(y: Int) extends FoldAlong

  case class Data(dots: List[Coord], foldAlongs: List[FoldAlong]) {
    val maxX: Int = if (dots.isEmpty) 0 else dots.map(_.x).max + 1
    val maxY: Int = if (dots.isEmpty) 0 else dots.map(_.y).max + 1

    def render(): Unit = {
      val grid = Array.fill(maxY, maxX)('.')

      dots.foreach { case Coord(x, y) =>
        grid(y)(x) = '#'
      }

      grid.foreach(ys =>
        println(ys.mkString)
      )
    }

    def foldAlong(): Data = {
      foldAlongs match {
        case Nil => this
        case FoldVerticalX(xx) :: fas =>
          val dots2 = dots
            .map {
              case c@Coord(x, _) if x <= xx => c
              case Coord(x, y) => Coord(xx - (x - xx), y)
            }
            .distinct
          Data(dots2, fas)

        case FoldHorizontalY(yy) :: fas =>
          val dots2 = dots
            .map {
              case c@Coord(_, y) if y <= yy => c
              case Coord(x, y) => Coord(x, yy - (y - yy))
            }
            .distinct
          Data(dots2, fas)
      }
    }
  }

  val PDot = """^(\d+),(\d+)$""".r
  val PFoldX = """^fold along x=(\d+)$""".r
  val PFoldY = """^fold along y=(\d+)$""".r

  def parse(lines: List[String]): Data = {
    val d1 = lines.foldLeft(Data(Nil, Nil)) {
      case (acc, PDot(x, y)) => acc.copy(dots = Coord(x.toInt, y.toInt) :: acc.dots)
      case (acc, PFoldX(x)) => acc.copy(foldAlongs = FoldVerticalX(x.toInt) :: acc.foldAlongs)
      case (acc, PFoldY(y)) => acc.copy(foldAlongs = FoldHorizontalY(y.toInt) :: acc.foldAlongs)
      case (acc, _) => acc
    }
    Data(d1.dots.reverse, d1.foldAlongs.reverse)
  }

  def part1(): Int = {
    val data = parse(in)

    val data2 = data.foldAlong()
//    data2.render()
    data2.dots.length
  }

  def part2(): Unit = {
    @tailrec
    def rec(data: Data) : Data = {
      data.foldAlongs match {
        case Nil => data
        case _ => rec(data.foldAlong())
      }
    }
    val data = parse(in)
    val end = rec(data)
    end.render()
  }

  println(part1())
  part2()
}
