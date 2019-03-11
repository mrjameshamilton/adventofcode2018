import org.scalatest.FlatSpec

import scala.io.Source

class AppSpec extends FlatSpec {

  private val sleepRecords = App.parse(Source.fromResource("input.txt").getLines().toList)

  "part 1 example" should "be 240" in {
    assert(App.part1(sleepRecords) == 240)
  }

  "part 2 example" should "be 4455" in {
    assert(App.part2(sleepRecords) == 4455)
  }
}
