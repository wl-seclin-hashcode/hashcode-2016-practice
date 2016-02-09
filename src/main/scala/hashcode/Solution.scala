package hashcode

import scala.collection.IndexedSeq

case class Solution(commands: IndexedSeq[Command])

trait Command {
  def update(picture: Map[Point, Boolean], problem: Problem): Map[Point, Boolean]
}

case class Point(row: Int, col: Int) {
  override def toString = s"$row $col"
}

case class PaintSquare(p:Point, size: Int) extends Command {

  override def toString = s"PAINT_SQUARE $p $size"

  def update(picture: Map[Point, Boolean], problem: Problem): Map[Point, Boolean] = {
    val row0 = p.row - size
    val col0 = p.col - size
    assert(row0 >= 0)
    assert(col0 >= 0)
    val rown = p.row + size
    val coln = p.col + size
    assert(rown < problem.nrow, s"$this row $rown")
    assert(coln < problem.ncol, s"$this col $coln")
    val area = for (i <- row0 to rown; j <- col0 to coln) yield (i, j)
    area.foldLeft(picture) {
      case (pic, (i, j)) => pic.updated(Point(i, j), true)
    }
  }
}
case class Erase(row: Int, col: Int) extends Command {
  override def toString = s"ERASE_CELL $row $col"

  def update(picture: Map[Point, Boolean], problem: Problem): Map[Point, Boolean] = {
    assert(row < problem.nrow)
    assert(col < problem.ncol)
    picture.updated(Point(row, col), false)
  }
}

class PaintLine(p1: Point, p2: Point) extends Command {

  override def toString = s"PAINT_LINE $p1 $p2"

  override def update(picture: Map[Point, Boolean], problem: Problem) = ???
}