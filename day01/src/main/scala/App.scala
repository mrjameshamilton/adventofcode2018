import scala.annotation.tailrec
import scala.collection.immutable.HashSet
import scala.io.Source

object App {
  def part1(input : List[Int]): Int = input.sum

  def part2(input : List[Int]): Int = {
    @tailrec def _part2(input : List[Int], current : Int, seen : HashSet[Int]) : Int =
      if (seen.contains(current)) current else _part2(input.tail :+ input.head, current + input.head, seen + current)
    _part2(input, 0, HashSet())
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("input.txt").getLines().map(_.toInt).toList
    println(part1(input))
    println(part2(input))
  }
}