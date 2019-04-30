import org.scalatest.FlatSpec

class AppSpec extends FlatSpec {

  val input = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

  "example" should "parse tree" in {
    assert(TreeParser.parse(input).get ==  Node(List(Node(List(),List(10, 11, 12)), Node(List(Node(List(),List(99))),List(2))),List(1, 1, 2)))
  }

  "part 1 example" should "be 138" in {
    assert(App.part1(TreeParser.parse(input).get) == 138)
  }

  "part 2 example" should "be 66" in {
    assert(App.part2(TreeParser.parse(input).get) == 66)
  }
}
