import scala.io.Source

object App {

  def part1(input: List[String]) : Int = input.count(s => hasExactly(s, 2)) * input.count(s => hasExactly(s, 3))

  def part2(input : List[String]) : String = {
    for (a <- input) {
      for (b <- input) {
        if (difference(a, b) == 1) {
          return a.toList.intersect(b.toList).mkString
        }
      }
    }
    ""
  }

  def hasExactly(s : String, n : Int) : Boolean = s.groupBy(_.toChar).count(_._2.length == n) >= 1

  def difference(a : String, b : String) : Int = {
    var count = 0
    for (i <- 0 until a.length) {
      if (a.charAt(i) == b.charAt(i)) count = count + 1
    }
    a.length - count
  }

  def main(args: Array[String]): Unit = {
    println(part1(Source.fromResource("input.txt").getLines().toList))
    println(part2(Source.fromResource("input.txt").getLines().toList))
  }
}
