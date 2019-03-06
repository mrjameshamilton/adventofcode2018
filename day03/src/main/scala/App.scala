import scala.collection.mutable.ListBuffer
import scala.io.Source

case class Claim(n: Int, x:Int, y:Int, w:Int, h:Int) {
  def x2: Int = x + w
  def y2: Int = y + h
  def overlaps(r2:Claim) : Boolean = x < r2.x2 && x2 > r2.x && y < r2.y2 && y2 > r2.y
  def points: List[(Int, Int)] = {
    val l = new ListBuffer[(Int, Int)]()
    for (i <- x.until(x2)) {
      for (j <- y.until(y2)) {
        l += ((i, j))
      }
    }
    l.toList
  }
}

object App {

  def toClaim(s:String) : Claim = {
    val pattern = "^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$".r
    val pattern(n, x, y, w, h) = s
    Claim(n.toInt, x.toInt, y.toInt, w.toInt, h.toInt)
  }

  //what is the total area of fabric with overlaps?
  def part1(lines:List[String]): Int = lines.flatMap(toClaim(_).points).groupBy(identity).mapValues(_.size).count(_._2 > 1)

  //what is the single claim that doesn't overlap with any other?
  def part2(lines:List[String]): Int = {
    val claims = lines.map(toClaim)
    val listBuffer = new ListBuffer[Claim]()
    for (x <- claims; y <- claims if x != y) {
      if (x overlaps y) listBuffer += x
    }

    claims.diff(listBuffer).head.n
  }

  def main(args: Array[String]): Unit = {
    println(part1(Source.fromResource("input.txt").getLines().toList))
    println(part2(Source.fromResource("input.txt").getLines().toList))
  }
}


