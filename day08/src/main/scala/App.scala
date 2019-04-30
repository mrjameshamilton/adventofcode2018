import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object App {

  /**
    * Sum of all metadata.
    *
    * @param tree
    * @return
    */
  def part1(tree: Node): Int = {
    @tailrec
    def _part1(toVisit: List[Node], sum: Int): Int = toVisit match {
      case Nil => sum
      case head :: tail => _part1(head.children ++ tail, sum + head.metadata.sum)
    }
    _part1(List(tree), 0)
  }

  /**
    * Sum of a node is:
    *  - no children: sum of metadata
    *  - children: metadata values are child indices (starting at index 1) - these are the "new children"
    * @param tree
    * @return
    */
  def part2(tree: Node): Int = {
    val cache = mutable.HashMap[Node, Int]()
    def _part2(node:Node): Int = {
        if (!cache.contains(node)) {
          if (node.children.nonEmpty) {
            val sum = node.metadata
              .filterNot(i => i == 0 || i > node.children.size)
              .map(i => Some(node.children(i - 1)))
              .collect { case child if child.isDefined => child.get }
              .foldLeft(0)(_ + _part2(_))
            cache.put(node, sum)
          } else {
            cache.put(node, node.metadata.sum)
          }
        }
        cache(node)
    }
    _part2(tree)
  }

  def main(args: Array[String]): Unit = {
    val s = Source.fromResource("input.txt").getLines().mkString("\n")
    val tree = TreeParser.parse(s)
    if (tree.successful) {
      println(part1(tree.get))
      println(part2(tree.get))
    } else {
      println(tree)
    }
  }
}
