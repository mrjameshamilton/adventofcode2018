import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source


object App {

  def parse(lines: List[String]): List[(Char, Char)] = lines.map(parse)

  def parse(line: String): (Char, Char) = {
    val pattern = "Step (\\D) must be finished before step (\\D) can begin.".r
    val pattern(a, b) = line
    (a(0), b(0))
  }

  def part1(edges: List[(Char, Char)]): String = {
    val nodesA = edges.map(_._1).distinct
    val nodesB = edges.map(_._2).distinct
    val startNodes = nodesA diff nodesB //the nodes with no incoming edges

    val result = new ListBuffer[Char]
    val allEdges = edges.to[ListBuffer]
    //use a priority queue to ensure that characters are processed in alphabetical order
    val queue = new mutable.PriorityQueue[Char]()(Ordering.Char.reverse)
    startNodes.foreach(queue.enqueue(_))

    while (queue.nonEmpty) {
      val n = queue.dequeue
      result += n
      allEdges.filter(n == _._1).foreach {
        case (_, m) =>
          allEdges -= ((n, m))
          if (allEdges.count(m == _._2) == 0) queue.enqueue(m)
      }
    }
    result.mkString("")
  }

  //credit to https://github.com/pindab0ter/Advent-of-Code-2018-Kotlin/blob/master/Day%207%20-%20The%20Sum%20of%20Its%20Parts/src/nl/pindab0ter/aoc2018/day7/SumOfItsParts.kt
  //for inspiration for the part 2 solution!
  def part2(edges: List[(Char, Char)], numberOfWorkers: Int = 1, extraTime: Int = 0): Int = {
    //create a child-parent dependencies map
    val adj = edges.groupBy(_._2).mapValues(pairs => pairs.map(pair => pair._1).toSet)
    val nodesA = edges.map(_._1).distinct
    val nodesB = edges.map(_._2).distinct
    val allTasks = (nodesA union nodesB).distinct
    val completedTasks = mutable.Set[Char]()
    val inProgressTasks = mutable.Map[Char, Int]()
    var idleWorkerCount = numberOfWorkers
    var totalTime = 0

    while (completedTasks.size < allTasks.size) {
      inProgressTasks.filter(_._2 == totalTime).keys.foreach(c => {
        inProgressTasks.remove(c)
        completedTasks.add(c)
        idleWorkerCount += 1
      })

      if (idleWorkerCount > 0) {
        allTasks
          .filterNot(c => completedTasks.contains(c) || inProgressTasks.contains(c))
          .filter(c => hasNoDependencies(c) || dependenciesAlreadyComplete(c))
          .sorted
          .take(idleWorkerCount)
          .foreach(c => {
            inProgressTasks(c) = totalTime + timeToRun(c)
            idleWorkerCount -= 1
          })
      }

      totalTime += 1
    }

    def timeToRun(c: Char): Int = c - 'A' + 1 + extraTime

    def hasNoDependencies(c: Char): Boolean = !adj.contains(c)

    def dependenciesAlreadyComplete(c: Char): Boolean = adj(c).subsetOf(completedTasks)

    totalTime - 1
  }


  def main(args: Array[String]): Unit = {
    println(part1(parse(Source.fromResource("input.txt").getLines().toList)))
    println(part2(parse(Source.fromResource("input.txt").getLines().toList), 5, 60))
  }
}
