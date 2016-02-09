package hashcode

case class Problem(picture: Vector[String], nrow: Int, ncol: Int) {

  lazy val points = for {
    (row, line) ← picture.zipWithIndex
    (c, col) ← row.zipWithIndex
    if c == '#'
  } yield Point(line, col)

}

case class Shape(points: Set[Point]) {
  lazy val minRow = points.minBy(_.row).row
  lazy val minCol = points.minBy(_.col).col
  lazy val maxRow = points.maxBy(_.row).row
  lazy val maxCol = points.maxBy(_.col).col

  lazy val lines = for {
    (_, pts) ← points.groupBy(_.row).toList.sortBy(_._1)
    line ← groups { p ⇒ p.copy(col = p.col + 1) }(pts.toList.sortBy(_.col))
  } yield line

  lazy val cols = for {
    (_, pts) ← points.groupBy(_.col).toList.sortBy(_._1)
    col ← groups { p ⇒ p.copy(row = p.row + 1) }(pts.toList.sortBy(_.row))
  } yield col

  private def groups(next: Point ⇒ Point)(pts: List[Point], group: List[Point] = Nil, acc: List[List[Point]] = Nil): List[List[Point]] = pts match {
    case Nil ⇒ if (group.isEmpty) acc else group :: acc
    case h :: t ⇒
      if (group.isEmpty || h == next(group.head)) groups(next)(t, h :: group, acc)
      else groups(next)(t, List(h), group :: acc)
  }
}

