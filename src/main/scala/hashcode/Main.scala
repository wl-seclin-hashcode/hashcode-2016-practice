package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success
import grizzled.slf4j.Logging

object Main extends App with Logging {
  val names = Seq(
    "learn_and_teach.in",
    "logo.in",
    "right_angle.in")
  info(s"total score for $names : " + names.map(solveIt).sum)

  def solveIt(n: String) = {
    val problem = Parser.read(n)
    val solution = Solver.solve(problem)
    Validator.score(solution, problem)
      .recover { case e => error(s"validation error for $n", e); 0 }
      .map { score =>
        info(s"score for $n : $score")
        Formatter.write(solution, score, n)
        score
      }.get
  }

}