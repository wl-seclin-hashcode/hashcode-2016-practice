package hashcode

import grizzled.slf4j.Logging
import scala.annotation.tailrec
import scala.math._

object Solver extends Logging {
  def solve(problem: Problem): Solution = {
    import problem._
    val shapes = findShapes(problem.points)

    info(s"${shapes.size} shapes")

    val shapeCmds = for {
      shape ← shapes.toList
      lines = paintShape(shape)
      _ = debug(s"${lines.size} moves to paint $shape")
      cmd ← lines
    } yield cmd
    Solution(shapeCmds.toVector)
  }

  def squaresInShape(shape: Shape) = {
    import shape._
    val maxSize = ((maxRow - minRow) min (maxCol - minCol))
    for {
      halfSize ← 1 to maxSize / 2
      size = 2 * halfSize + 1
      maxCells = size * size
      r ← minRow + halfSize to maxRow - halfSize
      c ← minCol + halfSize to maxCol - halfSize
      count = shape.points.count(p ⇒ p.col >= c - halfSize && p.col <= c + halfSize && p.row >= r - halfSize && p.row <= r + halfSize)
      if count == maxCells
    } yield PaintSquare(Point(r, c), halfSize)
  }.toList

  val shapeCache = collection.mutable.Map.empty[Shape, List[Command]]

  def paintShape(shape: Shape): List[Command] =
    shapeCache.getOrElseUpdate(shape, {
      val largeSquares = {
        val squares = squaresInShape(shape).groupBy(_.size)
        if (squares.isEmpty) Nil else squares.maxBy(_._1)._2
      }
      debug(s"${largeSquares.size} largest squares in shape ${shape.hashCode}")
      val lineCommands = paintShapeWithLines(shape)
      debug(s"${lineCommands.size} lines to paint shape ${shape.hashCode}")

      val candidates = largeSquares.headOption.toList ++ largeSquares.lastOption.toList
      val paintWithSquares = if (shape.points.size < 400) candidates.map {
        case cmd @ PaintSquare(Point(r, c), halfSize) ⇒
          val rest = shape.points.filterNot(p ⇒ abs(p.col - c) <= halfSize && abs(p.row - r) <= halfSize)
          val cmds = findShapes(rest).toList.flatMap(paintShape)
          cmd :: cmds
      }
      else List(shape.split.flatMap(paintShape))
      val options = lineCommands :: paintWithSquares
      options.sortBy(_.size).head
    })

  def paintShapeWithLines(shape: Shape, acc: List[Command] = Nil): List[Command] =
    if (shape.points.isEmpty) acc
    else {
      def countMandatorySections(part: List[Point], orth: List[List[Point]]) = {
        for {
          point ← part
          if !orth.exists(line ⇒ line.size > 1 && line.contains(point))
        } yield 1
      }.sum

      val countedRows = shape.lines.map(l ⇒ (true, l, countMandatorySections(l, shape.cols)))
      val countedCols = shape.cols.map(l ⇒ (false, l, countMandatorySections(l, shape.lines)))
      val mandatory = for {
        (horiz, line, count) ← countedCols ++ countedRows
        if count > 0
      } yield (line, paintLine(horiz, line))
      if (mandatory.nonEmpty) {
        val cmds = mandatory.map(_._2)
        debug(s"mandatory : $cmds")
        val rest = shape.points -- mandatory.map(_._1).flatten
        cmds ++ acc ++ findShapes(rest).flatMap(sh ⇒ paintShapeWithLines(sh, Nil))
      } else {
        debug(s"only ${acc.size} mandatory commands, fallback to vert/horiz mode")
        val horiz = paintShapeHoriz(shape)
        val vert = paintShapeVert(shape)
        val lines = Seq(horiz, vert).minBy(_.size)
        acc ++ lines
      }
    }

  def paintLine(horiz: Boolean, line: List[Point]) =
    if (horiz) PaintLine(line.minBy(_.col), line.maxBy(_.col))
    else PaintLine(line.minBy(_.row), line.maxBy(_.row))

  def paintShapeHoriz(shape: Shape): List[Command] = for {
    line ← shape.lines.toList
  } yield paintLine(true, line)

  def paintShapeVert(shape: Shape): List[Command] = for {
    col ← shape.cols.toList
  } yield paintLine(false, col)

  def findShapes(points: Iterable[Point]): Set[Shape] = {
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