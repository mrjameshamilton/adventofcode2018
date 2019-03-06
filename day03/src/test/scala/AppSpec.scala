import org.scalatest.FlatSpec

import scala.io.Source

class AppSpec extends FlatSpec {

  "parse square" should "be a Rect" in {
    assert(App.toClaim("#1 @ 555,891: 18x12") == Claim(1, 555,891,18,12))
  }

  "points" should "be within rect" in {
    assert(Claim(1, 1,3,4,4).points == List((1,3), (1,4), (1,5), (1,6), (2,3), (2,4), (2,5), (2,6), (3,3), (3,4), (3,5), (3,6), (4,3), (4,4), (4,5), (4,6)))
  }

  "example" should "be 4" in {
    assert(App.part1(Source.fromResource("input.txt").getLines().toList) == 4)
  }

  "part 2 example " should "be 3" in {
    assert(App.part2(Source.fromResource("input.txt").getLines().toList) == 3)
  }
}
