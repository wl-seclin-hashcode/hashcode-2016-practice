package painting

import java.io.{PrintStream, File}
import java.util.Scanner

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 17/11/15
 * Time: 12:23
 */
object Problem {
  val scan = new Scanner(new File("doodle.txt"))
  import scan._
  val nrow = nextInt()
  val ncol = nextInt()
  nextLine()
  val picture = Array.fill(nrow) { nextLine() }
}

trait Command
case class Paint(row: Int, col: Int, size: Int) extends Command {
  override def toString = s"PAINTSQ $row $col $size"
}
case class Erase(row: Int, col: Int) extends Command {
  override def toString = s"ERASECELL $row $col"
}


object Main extends App {
  def validate(commands: List[Command]) = {

  }

  def output(sol: IndexedSeq[Command]) = {
    val f = new PrintStream(s"/tmp/out.${sol.length}.txt")
    f.println(sol.length)
    f.println(sol.mkString("\n"))
    f.close
  }

  //println(Problem.picture.mkString("\n"))
  val sol = Solver.solve
  output(sol)
  println(s"Score: ${sol.length}")
}

