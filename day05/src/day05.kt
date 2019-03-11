import java.util.*

fun main() {
    val example = "dabAcCaCBAcCcaDA".toList()
    println("Part 1 example: " + part1(example) + ", expected: " + 10)
    println("Part 2 example: " + part2(part1(example)) + ", expected: " + 4)

    val part1Result = part1(object {}.javaClass.getResource("input.txt").readText().trim().toList())
    println(part1Result.size)
    println(part2(part1Result))
}

fun part1(list:List<Char>): List<Char> {
    val stack = Stack<Char>()
    for (c in list) {
        var popped = false
        if (!stack.isEmpty() && stack.peek() != c && stack.peek().toUpperCase() == c.toUpperCase()) {
            stack.pop()
            popped = true
        }

        if (!popped) stack.push(c)
    }
    return stack.toList()

}

fun part2(list:List<Char>) : Int {
    var min = list.size
    for (c in 'a'..'z') {
        val size = part1(list.filterNot { it == c || it == c.toUpperCase() }).size
        if (size < min) min = size
    }
    return min
}
