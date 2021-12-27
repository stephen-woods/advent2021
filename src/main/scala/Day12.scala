import scala.collection.mutable
import scala.io.Source

object Day12 extends App {

  val source = Source.fromFile("day12input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end""".stripMargin.split("\\n").toList

  val input3 =
    """dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc""".stripMargin.split("\\n").toList

  val input4 =
    """fs-end
      |he-DX
      |fs-he
      |start-DX
      |pj-DX
      |end-zg
      |zg-sl
      |zg-pj
      |pj-he
      |RW-he
      |fs-DX
      |pj-RW
      |zg-RW
      |start-pj
      |he-WI
      |zg-he
      |pj-fs
      |start-RW""".stripMargin.split("\\n").toList

  val in = input


  val graph = in
    .map(_.split('-').toList)
    .foldLeft(Map.empty[String, Set[String]]){
      case (acc, "start" :: x :: Nil) =>
        val val0 = acc.getOrElse("start", Set.empty) + x
        acc + ("start" -> val0)
      case (acc, x :: "start" :: Nil) =>
        val val0 = acc.getOrElse("start", Set.empty) + x
        acc + ("start" -> val0)
      case (acc, x :: y :: Nil) =>
        val val0 = acc.getOrElse(x, Set.empty) + y
        val val1 = acc.getOrElse(y, Set.empty) + x
        acc + (x -> val0) + (y -> val1)
    }

  type Path = List[String]
  type Graph = Map[String, Set[String]]


  // Small cave once
  def rec1(g: Graph, pos: String, path: Path): List[Path] = {
    pos match {
      case "end" => ("end" :: path).reverse :: Nil
      case x =>
        g.get(x) match {
          case None => Nil
          case Some(nexts) =>
            val g1 = if (x.forall(_.isLower)) g - x else g
            nexts.toList.sorted.flatMap{ n =>
              rec1(g1, n, x :: path)
            }
        }
    }
  }

  def rec2(g: Graph, pos: String, path: Path, twice: Boolean): List[Path] = {
    pos match {
      case "end" => ("end" :: path).reverse :: Nil
      case x if x.forall(_.isLower) =>
        val nexts = g(x)
        (twice, path.contains(x)) match {
          case (true, true) => Nil
          case (false, true) =>
            nexts.toList.sorted.flatMap { n =>
              rec2(g, n, x :: path, true)
            }
          case _ =>
            nexts.toList.sorted.flatMap { n =>
              rec2(g, n, x :: path, twice)
            }
        }

      case x =>
        val nexts = g(x)
        nexts.toList.sorted.flatMap { n =>
          rec2(g, n, x :: path, twice)
        }
    }
  }


  def part1(): Int = {
    val paths = rec1(graph, "start", Nil)
    paths.map(_.mkString(",")).sorted.foreach(println)
    paths.length
  }

  def part2(): Int = {
    println(graph)
    println
    val paths = rec2(graph, "start",  Nil, false)
    paths.map(_.mkString(",")).foreach(println)
    paths.length
  }
  
  println(part1())
  println(part2())
}
