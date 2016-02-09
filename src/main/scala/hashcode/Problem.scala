package hashcode

import scala.annotation.tailrec

case class Problem(picture: Vector[String], nrow: Int, ncol: Int) {
  def update(row: Int, col: Int, c: Char) =
    copy(picture = picture.updated(row, picture(row).updated(col, c)))

  lazy val points = for {
    (row, line) <- picture.zipWithIndex
    (c, col) <- row.zipWithIndex
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
          dx <- -1 to 1
          dy <- -1 to 1
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

case class Shape(points: Set[Point])

