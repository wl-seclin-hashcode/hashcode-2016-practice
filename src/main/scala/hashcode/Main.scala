package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success

object Main extends App {
  val names = Seq("learn_and_teach.in", "logo.in", "right_angle.in")
  names.foreach(solveIt)

  def solveIt(n: String) = {
    val problem = Parser.read(n)
    val solution = Solver.solve(problem)
    Validator.score(solution, problem) match {
      case Success(score) =>
        println(s"score for $n : $score")
        Formatter.write(solution, score, n)
      case Failure(e) =>
        e.printStackTrace()
    }
  }

}