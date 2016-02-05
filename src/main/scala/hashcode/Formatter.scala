package hashcode

import java.io.PrintStream

object Formatter {
  def write(solution: Solution, score: Int, name: String): Unit = {
    val file = s"out.$name.$score.txt"
    val f = new PrintStream(file)
    f.println(solution.commands.size)
    f.println(solution.commands.mkString("\n"))
    f.close
    println(s"wrote to $file")
  }
}