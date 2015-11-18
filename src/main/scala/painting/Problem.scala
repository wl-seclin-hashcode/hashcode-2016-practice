package painting

import java.io.{ PrintStream, File }
import java.util.Scanner


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
