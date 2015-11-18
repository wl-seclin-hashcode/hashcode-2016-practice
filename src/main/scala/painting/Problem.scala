package painting

import java.io.{ PrintStream, File }
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
  val origpic = picture.clone
}

trait Command {
  def update(picture: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean]
}
case class Paint(row: Int, col: Int, size: Int) extends Command {
  override def toString = s"PAINTSQ $row $col $size"
  def update(picture: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = {
    val row0 = row - size
    val col0 = col - size
    assert(row0 >= 0)
    assert(col0 >= 0)
    val rown = row + size
    val coln = col + size
    assert(rown < Problem.nrow)
    assert(coln < Problem.ncol)
    val area = for (i <- row0 to rown; j <- col0 to coln) yield (i, j)
    area.foldLeft(picture) {
      case (pic, (i, j)) => pic.updated((i, j), true)
    }
  }
}
case class Erase(row: Int, col: Int) extends Command {
  override def toString = s"ERASECELL $row $col"

  def update(picture: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] =
    picture.updated((row, col), false)
}

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

