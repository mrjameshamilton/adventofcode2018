import org.scalatest.FlatSpec

import scala.io.Source

class AppSpec extends FlatSpec {

  val lines = Source.fromResource("input.txt").getLines().toList

  "parse line " should " be correct " in {
    assert(App.parse("Step C must be finished before step A can begin.") == ('C', 'A'))
  }

  "parse lines " should "return edge list" in {
    val expectedResult = List(('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E'))
    assert(App.parse(lines) == expectedResult)
  }

  "part 1 example" should "be CABDFE" in {
    assert(App.part1(App.parse(lines)) == "CABDFE")
  }

  "part 2 example" should "be 15" in {
    assert(App.part2(App.parse(lines), 2, 0) == 15)
  }

}
