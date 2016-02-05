package hashcode

import scala.collection.IndexedSeq

case class Solution(commands: IndexedSeq[Command])

trait Command {
  def update(picture: Map[(Int, Int), Boolean], problem: Problem): Map[(Int, Int), Boolean]
}

case class Paint(row: Int, col: Int, size: Int) extends Command {

  override def toString = s"PAINT_SQUARE $row $col $size"

  def update(picture: Map[(Int, Int), Boolean], problem: Problem): Map[(Int, Int), Boolean] = {
    val row0 = row - size
    val col0 = col - size
    assert(row0 >= 0)
    assert(col0 >= 0)
    val rown = row + size
    val coln = col + size
    assert(rown < problem.nrow, s"$this row $rown")
    assert(coln < problem.ncol, s"$this col $coln")
    val area = for (i <- row0 to rown; j <- col0 to coln) yield (i, j)
    area.foldLeft(picture) {
      case (pic, (i, j)) => pic.updated((i, j), true)
    }
  }
}
case class Erase(row: Int, col: Int) extends Command {
  override def toString = s"ERASE_CELL $row $col"

  def update(picture: Map[(Int, Int), Boolean], problem: Problem): Map[(Int, Int), Boolean] = {
    assert(row < problem.nrow)
    assert(col < problem.ncol)
    picture.updated((row, col), false)
  }
}
