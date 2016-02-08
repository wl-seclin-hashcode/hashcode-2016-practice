package hashcode

import java.io.PrintStream
import scala.util.Failure
import scala.util.Success
import grizzled.slf4j.Logging
import java.awt.Graphics
import java.awt.Dimension
import java.awt.Color

object Main extends App with Logging {
  val names = Seq(
    "learn_and_teach.in",
    "logo.in",
    "right_angle.in")
  info(s"total score for $names : " + names.map(solveIt).sum)

  def solveIt(n: String) = {
    val problem = Parser.read(n)
    val solution = Solver.solve(problem)
    showSolution(solution, problem)

    Validator.score(solution, problem)
      .recover { case e => error(s"validation error for $n", e); 0 }
      .map { score =>
        info(s"score for $n : $score")
        Formatter.write(solution, score, n)
        score
      }.get
  }

  def showSolution(s: Solution, p: Problem) {

    def paintSol(g: Graphics, d: Dimension, p: Problem, step: Int): Unit = {
      val cellWidth = (d.getWidth / p.ncol).toInt
      val cellHeight = (d.getHeight / p.nrow).toInt
      def coords(x: Int, y: Int): (Int, Int) =
        ((x * cellWidth).toInt,
          (y * cellHeight).toInt)

      for {
        cmd <- s.commands.take(step)
      } draw(cmd)

      def drawCell(r: Int, c: Int, erase: Boolean = false) = {
        val (x, y) = coords(c, r)
        if (erase) g.setColor(Color.white)
        else g.setColor(Color.BLACK)
        g.drawRect(x, y, cellWidth, cellHeight)
      }

      def draw(cmd: Command) = cmd match {
        case Paint(row, col, size) =>
          for {
            r <- row - size to row + size
            c <- col - size to col + size
          } drawCell(r, c)
        case Erase(row, col) =>
          drawCell(row, col, true)
      }

    }

    Visualizer(paintSol, (0 to s.commands.size).toSeq, p)

  }

}