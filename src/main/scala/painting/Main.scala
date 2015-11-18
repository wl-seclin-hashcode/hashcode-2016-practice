package painting

import java.io.PrintStream

object Main extends App {

  def output(sol: List[Command]) = {
    val name = s"out.${sol.length}.txt"
    val f = new PrintStream(name)
    f.println(sol.length)
    f.println(sol.mkString("\n"))
    f.close
    println(s"wrote to $name")
  }

  //println(Problem.picture.mkString("\n"))
  val sol = Solver.solve.toList
  output(sol)
  println(s"Score: ${sol.length}")
  Validator.validate(sol, Problem.origpic)
}