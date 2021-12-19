import java.lang.Integer.parseInt
import scala.annotation.tailrec
import scala.io.Source

object Day4 extends App {
  val source = Source.fromFile("day4input.txt")
  val input = source.getLines().toList
  source.close()

  val input2: List[String] =
    """7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
      |
      |22 13 17 11  0
      | 8  2 23  4 24
      |21  9 14 16  7
      | 6 10  3 18  5
      | 1 12 20 15 19
      |
      | 3 15  0  2 22
      | 9 18 13 17  5
      |19  8  7 25 23
      |20 11 10 24  4
      |14 21 16 12  6
      |
      |14 21 17 24  4
      |10 16 15  9 19
      |18  8 23 26 20
      |22 11 13  6  5
      | 2  0 12  3  7""".stripMargin.split("\n").toList

  case class BingoBoard(grid: Array[Array[Int]]) {
    def mark(Num: Int): BingoBoard = {
      val grid2 = grid
        .map {
          _.map {
            case Num => -1
            case x => x
          }
        }
      BingoBoard(grid2)
    }

    def isBingo: Boolean = {
      val rows = grid.exists(_.forall(x => x == -1))
      val cols = grid.transpose.exists(_.forall(x => x == -1))
      rows || cols
    }

    def score: Int = {
      grid.map {
        _.filter(_ != -1).sum
      }.sum
    }
  }

  object BingoBoard {
    def apply(lines: List[String]): BingoBoard = {
      val grid = lines
        .map {
          _
            .trim
            .split("\\s+")
            .map(parseInt)
        }
        .toArray
      new BingoBoard(grid)
    }
  }

  @tailrec
  def recRead(lines: List[String],
              bbs: List[BingoBoard]): List[BingoBoard] = {
    lines match {
      case Nil => bbs
      case "" :: tail => recRead(tail, bbs)
      case xs =>
        val bb = BingoBoard(xs.take(5))
        recRead(lines.drop(5), bb :: bbs)
    }
  }

  @tailrec
  def recPlay1(marks: List[Int], bbs: List[BingoBoard]): (Int, BingoBoard) = {
    val m = marks.head
    val bbs2 = bbs.map(_.mark(m))
    bbs2.filter(_.isBingo) match {
      case Nil => recPlay1(marks.tail, bbs2)
      case x :: _ => (m, x)
    }
  }

  @tailrec
  def recPlay2(marks: List[Int], bbs: List[BingoBoard]): (Int, BingoBoard) = {
    val m = marks.head
    val bbs2 = bbs.map(_.mark(m))
    val filtered = bbs2.filterNot(_.isBingo)

    filtered match {
      case Nil => (m, bbs2.head)
      case _ => recPlay2(marks.tail, filtered)
    }
  }


  val marks = input.head.split(',').map(_.toInt).toList
  val boards = recRead(input.tail, Nil).reverse

  def part1(): Int = {
    val (winningMark, winningBoard) = recPlay1(marks, boards)
    val winningScore = winningBoard.score
    winningMark * winningScore
  }

  def part2(): Int = {
    val (winningMark, winningBoard) = recPlay2(marks, boards)
    val winningScore = winningBoard.score
    winningMark * winningScore
  }

  println(part1())
  println(part2())
}
