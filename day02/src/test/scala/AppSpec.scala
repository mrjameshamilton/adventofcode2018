import org.scalatest.FlatSpec

import scala.io.Source

class AppSpec extends FlatSpec {

  "test input " should " produce 12" in {
    assert(App.part1(Source.fromResource("input.txt").getLines().toList) == 12)
  }

  "does not have exactly 2 a " should " be false" in {
    assert(App.hasExactly("aaabcfe", 2) == false)
  }

  "has exactly 2 a " should " be true" in {
    assert(App.hasExactly("aabcfe", 2) == true)
  }
  "abcde and axcye" should " differ by 2" in {
    assert(App.difference("abcde", "axcye") == 2)
  }

  "fghij and fguij" should "differ by 1" in {
    assert(App.difference("fghij", "fguij") == 1)
  }

  "part 2" should "be fgij" in {
    assert(App.part2(Source.fromResource("input2.txt").getLines().toList) == "fgij")
  }
}
