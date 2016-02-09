package hashcode

import grizzled.slf4j.Logging

object Solver extends Logging {
  def solve(problem: Problem): Solution = {
    import problem._

    info(s"${shapes.size} shapes")

    val shapeCmds = for {
      shape ← shapes.toList
      horiz = paintShapeHoriz(shape)
      vert = paintShapeVert(shape)
      lines = Seq(horiz, vert).minBy(_.size)
      _ = debug(s"${lines.size} moves to paint $shape")
      cmd ← lines
    } yield cmd
    Solution(shapeCmds.toVector)
  }

  def paintShapeHoriz(shape: Shape): List[Command] = for {
    line ← shape.lines.toList
  } yield PaintLine(line.minBy(_.col), line.maxBy(_.col))

  def paintShapeVert(shape: Shape): List[Command] = for {
    col ← shape.cols.toList
  } yield PaintLine(col.minBy(_.row), col.maxBy(_.row))

}