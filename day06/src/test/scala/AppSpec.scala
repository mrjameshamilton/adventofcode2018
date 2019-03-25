import org.scalatest.FlatSpec

import scala.io.Source

class AppSpec extends FlatSpec {

  val lines: List[String] = Source.fromResource("input.txt").getLines().toList

  "distance " should " be correct " in {
    assert(App.distance(Point(1, 1), Point(8, 9)) == 15)
  }

  "part 1 result " should " be 17" in {
    assert(App.part1(App.parse_lines(lines)) == 17)
  }

  "part 2 result " should " be 6" in {
    assert(App.part2(App.parse_lines(lines), 32) == 16)
  }
}
