import java.lang.Math.max
import scala.annotation.tailrec
import scala.io.Source

object Day5 extends App {
  val source = Source.fromFile("day5input.txt")
  val input = source.getLines().toList
  source.close()

  val input2: List[String] =
    """0,9 -> 5,9
      |8,0 -> 0,8
      |9,4 -> 3,4
      |2,2 -> 2,1
      |7,0 -> 7,4
      |6,4 -> 2,0
      |0,9 -> 2,9
      |3,4 -> 1,4
      |0,0 -> 8,8
      |5,5 -> 8,2""".stripMargin.split("\n").toList

  case class XY(x: Int, y: Int) {
    override def toString: String = s"$x,$y"
  }

  case class Line(start: XY, end: XY) {
    override def toString: String = s"$start -> $end"
  }

  class Map(grid: Array[Array[Int]]) {
    def render(): Unit = {
      grid.foreach{ line =>
        val s= line.map {
          case 0 => "."
          case x => x.toString
        } .mkString
        println(s)
      }
    }

    def draw(point: XY):Unit = {
      val x = point.x
      val y = point.y
      grid(y)(x) = grid(y)(x) + 1
    }

    def draw(line: Line): Unit = {

      def step(s: Int, e: Int ): Int = if (s < e) 1 else if (s == e) 0 else -1

      @tailrec
      def rec(curr: XY, end: XY, xstep: Int, ystep: Int): Unit = {
        draw(curr)
        if (curr != end) {
          rec(XY(curr.x + xstep, curr.y + ystep), end, xstep, ystep)
        }
      }

      val xstep = step(line.start.x, line.end.x)
      val ystep = step(line.start.y, line.end.y)

      rec (line.start, line.end, xstep, ystep)
    }

    def overlaps(): List[XY] = {
    val sss =  for {
      y <- grid.indices
      x <- grid.head.indices
     } yield {
       if (grid(y)(x) > 1) XY(x,y) :: Nil else Nil
     }
      sss.flatten.toList
    }
  }

  val LineRegEx = """^(\d+),(\d+) -> (\d+),(\d+)$""".r

  def readLine(in: String): Line = in match {
    case LineRegEx(x1, y1, x2, y2) =>
      Line(XY(x1.toInt, y1.toInt), XY(x2.toInt, y2.toInt))
  }

  def dimensions(lines: List[Line]): (Int, Int) = {
    lines
      .foldLeft((0, 0)) { case ((maxx, maxy), Line(start, end)) =>
        val maxx2 = max(max(maxx, start.x + 1), end.x + 1)
        val maxy2 = max(max(maxy, start.y + 1), end.y + 1)
        (maxx2, maxy2)
      }
  }
  val coords = input.map(readLine)
  val (maxx, maxy) = dimensions(coords)

  def part1(): Int = {
    val grid = Array.ofDim[Int](maxy, maxx)
    val map = new Map(grid)

    coords
      .filter(line => line.start.x == line.end.x || line.start.y == line.end.y)
      .foreach(map.draw)

    map.render()
    map.overlaps().length
  }

  def part2(): Int = {

    val grid = Array.ofDim[Int](maxy, maxx)
    val map = new Map(grid)

    coords.foreach(map.draw)
    map.render()
    map.overlaps().length
  }

  println(part1())
  println(part2())
}
