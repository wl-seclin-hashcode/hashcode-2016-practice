package hashcode

import java.io.PrintStream

object Formatter {
  def write(solution: Solution, score: Int): Unit = {
    val name = s"out.${score}.txt"
    val f = new PrintStream(name)
    f.println(solution.commands.mkString("\n"))
    f.close
    println(s"wrote to $name")
  }
}