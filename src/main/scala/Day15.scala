import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

case object Day15 extends App {


  val source = Source.fromFile("day15input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin.split("\n").toList

  type Grid = Array[Array[Int]]

  def parse(lines: List[String]): Grid = {
    lines.map {
      _.map {
        _.asDigit
      }.toArray
    }.toArray
  }

  val in = input

  case class Pos(x: Int, y: Int)

  case class Node(pos: Pos, risk: Int)

  val grid = parse(in)

  grid.map(_.mkString).foreach(println)

  val maxY = grid.length - 1
  val maxX = grid.head.length - 1
  val start = Pos(0, 0)
  val end = Pos(maxX, maxY)


  def avail(explored: mutable.Set[Pos], p: Pos): List[Pos] = {

    val left = p.copy(x = p.x - 1)
    val up = p.copy(y = p.y - 1)
    val right = p.copy(x = p.x + 1)
    val down = p.copy(y = p.y + 1)

    List(down, right, up, left)
      .filter(p => p.x >= 0 && p.x <= maxX)
      .filter(p => p.y >= 0 && p.y <= maxY)
      .filterNot(explored.contains)
  }

  def riskPath(froms: Array[Array[Pos]]): List[Pos] = {
    @tailrec
    def rec(acc: List[Pos], p: Pos): List[Pos] = {
      if (p == start) start :: acc
      else {
        val f = froms(p.y)(p.x)
        rec(p :: acc, f)
      }
    }

    rec(Nil, end).reverse
  }


  def part1(): Int = {
    val totalRisks = grid.map(_.map(_ => Int.MaxValue))
    totalRisks(start.y)(start.x) = 0

    val from = grid.map(_.map(_ => Pos(Int.MaxValue, Int.MaxValue)))
    from(start.y)(start.x) = start

    val explored = mutable.Set.empty[Pos]
    val enqueued = mutable.Set.empty[Pos]

    val pq = mutable.PriorityQueue[Pos]()(Ordering.by(p => -totalRisks(p.y)(p.x)))
    pq.enqueue(start)
    enqueued.add(start)

    while (pq.nonEmpty) {
      val p = pq.dequeue()
      explored.add(p)

      val nexts = avail(explored, p)

      nexts.foreach { pn =>
        val r = totalRisks(pn.y)(pn.x)
        val r1 = totalRisks(p.y)(p.x) + grid(pn.y)(pn.x)

        if (r1 < r) {
          totalRisks(pn.y)(pn.x) = r1
          from(pn.y)(pn.x) = p
        }
      }

      nexts
        .filterNot(enqueued.contains)
        .foreach{pn =>
          pq.enqueue(pn)
          enqueued.add(pn)
        }
    }

    val risk = totalRisks(end.y)(end.y)
//    val path = riskPath(from)
//    path.foreach(println)
    risk
  }

  println(part1())
}
