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

  val grid = parse(in)


  def avail(explored: mutable.Set[Pos],
            p: Pos,
            maxX: Int,
            maxY: Int): List[Pos] = {
    val left = p.copy(x = p.x - 1)
    val up = p.copy(y = p.y - 1)
    val right = p.copy(x = p.x + 1)
    val down = p.copy(y = p.y + 1)

    List(down, right, up, left)
      .filter(p => p.x >= 0 && p.x <= maxX)
      .filter(p => p.y >= 0 && p.y <= maxY)
      .filterNot(explored.contains)
  }

  def riskPath(froms: Array[Array[Pos]],
               start: Pos,
               end: Pos): List[Pos] = {
    @tailrec
    def rec(acc: List[Pos], p: Pos): List[Pos] = {
      if (p == start) start :: acc
      else {
        val f = froms(p.y)(p.x)
        rec(p :: acc, f)
      }
    }
    rec(Nil, end)
  }

  def bigGrid(risks: Array[Array[Int]]): Array[Array[Int]] = {
    val dimY = risks.length
    val dimX = risks.head.length

    val ret = Array.ofDim[Int](dimY * 5, dimX * 5)
    for {
      y <- risks.indices
      x <- risks(y).indices
      yy <- 0 to 4
      xx <- 0 to 4
    } {
      val r1 = risks(y)(x) + yy + xx
      val r2 = if (r1 > 9) r1 - 9 else r1
      ret(y + (yy * dimY))(x + (xx * dimX)) = r2
    }
    ret
  }

  /**
   * Variant of Dijkstra's algorithm to find path of least risk
   * @param risks risk based on position
   * @param start starting position
   *
   * @return totalRisks (mutable grid of least accumulated risk from start) and froms (mutable grid of positions
   *         indicating last least risk path)
   */
  def dijkstra(risks: Array[Array[Int]],
               start: Pos): (Array[Array[Int]], Array[Array[Pos]]) = {
    val maxY = risks.length - 1
    val maxX = risks.head.length - 1

    val totalRisks = risks.map(_.map(_ => Int.MaxValue))
    totalRisks(start.y)(start.x) = 0

    val froms = risks.map(_.map(_ => Pos(Int.MaxValue, Int.MaxValue)))
    froms(start.y)(start.x) = start

    // Priority queue of encountered, yet not fully explored positions ordered by least risk
    val pq = mutable.PriorityQueue[Pos]()(Ordering.by(p => -totalRisks(p.y)(p.x)))
    pq.enqueue(start)

    // Positions we have already explored, as indicated when positions are dequeued from the priority queue
    val explored = mutable.Set.empty[Pos]

    // Positions that have already been enqueued in the priority queue (so we don't have duplicates). Positions are
    // enqueued as we encounter them
    val enqueued = mutable.Set.empty[Pos]

    // Start off by enqueueing the start position
    enqueued.add(start)

    while (pq.nonEmpty) {
      val p = pq.dequeue()
      explored.add(p)

      // Find all available adjacent positions, excluding those we have already explored
      avail(explored, p, maxX, maxY)
        .tapEach { pn =>
          val r = totalRisks(pn.y)(pn.x)
          val r1 = totalRisks(p.y)(p.x) + risks(pn.y)(pn.x)

          if (r1 < r) {
            totalRisks(pn.y)(pn.x) = r1
            froms(pn.y)(pn.x) = p
          }
        }
        .filterNot(enqueued.contains)
        .foreach { pn =>
          pq.enqueue(pn)
          enqueued.add(pn)
        }
    }
    (totalRisks, froms)
  }


  def part1(): Int = {
    val risks = grid
//    risks.map(_.mkString).foreach(println)

    val maxY = risks.length - 1
    val maxX = risks.head.length - 1
    val start = Pos(0, 0)
    val end = Pos(maxX, maxY)

    val (totalRisks, froms) = dijkstra(risks, start)

    val endRisk = totalRisks(end.y)(end.y)
//    val path = riskPath(froms, start, end)
//    path.foreach(println)
    endRisk
  }

  def part2(): Int = {
    val risks = bigGrid(grid)
//    risks.map(_.mkString).foreach(println)

    val maxY = risks.length - 1
    val maxX = risks.head.length - 1
    val start = Pos(0, 0)
    val end = Pos(maxX, maxY)


    val (totalRisks, froms) = dijkstra(risks, start)

    val endRisk = totalRisks(end.y)(end.y)
//    val path = riskPath(froms, start, end)
//    path.foreach(println)
    endRisk
  }

  println(part1())
  println(part2())
}
