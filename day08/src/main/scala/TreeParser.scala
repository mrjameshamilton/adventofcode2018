import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

case class Node(children: List[Node], metadata: List[Int])

object TreeParser extends RegexParsers {

  override val whiteSpace: Regex = " ".r

  def integer: Parser[Int] = "\\d+".r ^^ { _.toInt }

  def tree: Parser[Node] = integer ~ integer >>
    { case numChildren ~ numMetadata => repN(numChildren, tree) ~ repN(numMetadata, integer) } ^^
    { case children ~ metadata => Node(children, metadata) }

  def parse(s: String): TreeParser.ParseResult[Node] = parseAll(tree, s)
}
