package hashcode

import scala.annotation.tailrec
import scala.collection.IndexedSeq

case class Problem(picture: Vector[String], nrow: Int, ncol: Int) {

  lazy val points = for {
    (row, line) ← picture.zipWithIndex
    (c, col) ← row.zipWithIndex
    if c == '#'
  } yield Point(line, col)

  lazy val shapes: Set[Shape] = {
    @tailrec
    def shapesRec(acc: Set[Shape] = Set.empty, pts: Set[Point] = points.toSet): Set[Shape] =
      if (pts.isEmpty) acc
      else {
        val h = pts.head
        val shape = Shape(reachable(pts, Set(h), Set(h)))
        shapesRec(acc + shape, pts -- shape.points)
      }

    def reachable(allowed: Set[Point], toExplore: Set[Point], acc: Set[Point]): Set[Point] =
      if (allowed.isEmpty || toExplore.isEmpty) acc
      else {
        val p = toExplore.head
        val next = for {
          dx ← -1 to 1
          dy ← -1 to 1
          if dx != 0 || dy != 0
          row = p.row + dx
          col = p.col + dy
          point = Point(row, col)
          if allowed.contains(point)
          if !acc.contains(point)
        } yield point
        reachable(allowed -- toExplore, toExplore - p ++ next, acc ++ next)
      }

    shapesRec()
  }
}

case class Shape(points: Set[Point]) {
  val minRow = points.minBy(_.row).row
  val minCol = points.minBy(_.col).col
  val maxRow = points.maxBy(_.row).row
  val maxCol = points.maxBy(_.col).col

  def lines = for {
    (_, pts) ← points.groupBy(_.row).toList.sortBy(_._1)
    line ← groups { p ⇒ p.copy(col = p.col + 1) }(pts.toList.sortBy(_.col))
  } yield line

  def cols = for {
    (_, pts) ← points.groupBy(_.col).toList.sortBy(_._1)
    col ← groups { p ⇒ p.copy(row = p.row + 1) }(pts.toList.sortBy(_.row))
  } yield col

  def groups(next: Point ⇒ Point)(pts: List[Point], group: List[Point] = Nil, acc: List[List[Point]] = Nil): List[List[Point]] = pts match {
    case Nil ⇒ if (group.isEmpty) acc else group :: acc
    case h :: t ⇒
      if (group.isEmpty || h == next(group.head)) groups(next)(t, h :: group, acc)
      else groups(next)(t, List(h), group :: acc)
  }

  def image: IndexedSeq[IndexedSeq[Boolean]] = {
    for {
      row ← minRow to maxRow
    } yield for {
      col ← minCol to maxCol
      point = Point(row, col)
    } yield points.contains(point)
  }
}

