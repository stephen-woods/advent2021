import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App {

  val source = Source.fromFile("day10input.txt")
  val input = source.getLines().toList
  source.close()

  val input2 =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\\n").toList

  val in = input

  @tailrec
  def rec(cs: List[Char], stack: List[Char]): Either[(Char, List[Char]), List[Char]] = {
    (cs, stack) match {
      case (Nil, _) => Right(stack)
      case ('(' :: xs, _) => rec(xs, '(' :: stack)
      case ('[' :: xs, _) => rec(xs, '[' :: stack)
      case ('{' :: xs, _) => rec(xs, '{' :: stack)
      case ('<' :: xs, _) => rec(xs, '<' :: stack)
      case (')' :: xs, '(' :: ys) => rec(xs, ys)
      case (']' :: xs, '[' :: ys) => rec(xs, ys)
      case ('}' :: xs, '{' :: ys) => rec(xs, ys)
      case ('>' :: xs, '<' :: ys) => rec(xs, ys)
      case (x :: _, _) => Left(x, stack)
    }
  }

  def part1(): Int = {
    in
      .map(s => rec(s.toList, Nil))
      .collect { case Left((c, _)) => c }
      .map {
        case ')' => 3
        case ']' => 57
        case '}' => 1197
        case '>' => 25137
      }
      .sum
  }

  def completedBy(stack: List[Char]) : List[Char] = {
    stack.map{
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }
  }

  def score(cs: List[Char]): Long = {
    cs.foldLeft(0L){
      case (acc, ')') => (acc * 5) + 1
      case (acc, ']') => (acc * 5) + 2
      case (acc, '}') => (acc * 5) + 3
      case (acc, '>') => (acc * 5) + 4

    }
  }

  def part2(): Long = {
    val cs_ss = in
      .map(s => rec(s.toList, Nil))
      .collect { case Right(stack) => stack}
      .map(completedBy)
      .map(cs => (cs, score(cs)))

    //cs_ss.foreach{ case (cs, s) => println(s" - ${cs.mkString} - $s total points")}

    val sorted_cs_ss = cs_ss.sortBy{case (cs, s) => s }

    // sorted_cs_ss.foreach{ case (cs, s) => println(s" - ${cs.mkString} - $s total points")}

    val middle = sorted_cs_ss.length / 2
    sorted_cs_ss(middle)._2
  }

  println(part1())
  println(part2())
}
