package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success

object Main extends App {
  val names = Seq("learn_and_teach.in", "logo.in", "right_angle.in")
  println(s"total score for $names : " + names.map(solveIt).sum)

  def solveIt(n: String) = {
    val problem = Parser.read(n)
    val solution = Solver.solve(problem)
    Validator.score(solution, problem).recover { case e => e.printStackTrace(); 0 }.map { score =>
      println(s"score for $n : $score")
      Formatter.write(solution, score, n)
      score
    }.get
  }

}