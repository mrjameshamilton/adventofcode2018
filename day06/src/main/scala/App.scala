import java.lang.Math._

import scala.io.Source

case class Point(x: Int, y: Int) {
  override def toString: String = f"($x, $y)"
}

object App {

  def main(args: Array[String]): Unit = {
    println(part1(parse_lines(Source.fromResource("input.txt").getLines().toList)))
    println(part2(parse_lines(Source.fromResource("input.txt").getLines().toList), 10000))
  }

  def parse_lines(lines: List[String]): List[Point] = {
    lines.map(_.split(',')).map(p => Point(p(0).toInt, p(1).trim.toInt))
  }

  def part1(points: List[Point]): Int = {
    val (max_x, max_y, min_x, min_y) = get_boundaries(points)
    val closestPoints = generate_points(max_x, max_y, min_x, min_y)
      .map(point => (point, closest(points, point)))
      .filter(_._2.isDefined) //if there is a unique closest point
      .map(x => (x._1, x._2.get))
      .groupBy(_._2)
      .map(x => x._1 -> x._2.map(_._1).toList)

    val infiniteAreaPoints = closestPoints.filter(_._2.count(p => p.x == min_x || p.x == max_x || p.y == min_y || p.y == max_y) > 0).keys.toList

    closestPoints.filterKeys(!infiniteAreaPoints.contains(_))
      //return the biggest area
      .maxBy(_._2.size)._2.size
  }

  def generate_points(max_x: Int, max_y: Int, min_x: Int, min_y: Int): Seq[Point] = for (x <- min_x to max_x; y <- min_y to max_y) yield Point(x, y)

  def get_boundaries(points: List[Point]): (Int, Int, Int, Int) =
    (points.maxBy(_.x).x, points.maxBy(_.y).y, points.minBy(_.x).x, points.minBy(_.y).y)

  def closest(points: List[Point], otherPoint: Point): Option[Point] = {
    val distances = points.groupBy(p => distance(p, otherPoint))
    val shortest = distances.minBy(_._1)._1
    distances.filter(x => x._2.size == 1).filter(x => x._1 == shortest).map(x => x._2.head).headOption
  }

  def distance(a: Point, b: Point): Int = abs(b.x - a.x) + abs(b.y - a.y)

  def part2(points: List[Point], maxDistance: Int): Int = {
    (generate_points _).tupled(get_boundaries(points))
      .map(point => points.foldLeft(0)(_ + distance(point, _))).count(_ < maxDistance)
  }
}
