package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success

object Main extends App {
  val names = Seq("logo.in", "learn_and_teach.in", "right_angle.in")
  names.foreach(solveIt)

  def solveIt(n: String) = {
    val problem = Parser.read(n)
    val solution = Solver.solve(problem.createCopy)
    Validator.score(solution, problem) match {
      case Success(score) =>
        println(s"score for $n : $score")
        Formatter.write(solution, score, n)
      case Failure(e) =>
        e.printStackTrace()
    }
  }

}