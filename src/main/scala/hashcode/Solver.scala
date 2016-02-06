package hashcode

object Solver {
  def solve(problem: Problem): Solution = {
    import problem._
    var rest=problem

    def paintArea(halfLength: Int, stop: Int, full: Boolean): IndexedSeq[Command] = {
      val length = 2 * halfLength + 1
      def shouldPaint(area: Vector[String]) =
        if (full)
          area.forall(_.forall('#'.==))
        else
          area.map(_.count('#'.==)).sum > halfLength * area.map(_.count('.'.==)).sum + 1

      if (length <= stop) IndexedSeq.empty[Command]
      else {
        val commands = for {
          row ← 0 until nrow by length
          if row + length <= nrow
          col ← 0 until ncol by length
          if col + length <= ncol
        } yield {
          val area = rest.picture
            .slice(row, row + length)
            .map(_.slice(col, col + length))

          if (area.nonEmpty && shouldPaint(area) && area.length == area.head.length) {
            val erases = for {
              r1 ← area.indices
              c1 ← area(r1).indices
              if area(r1)(c1) == '.'
            } yield Erase(row + r1, col + c1)

            for {
              r1 ← area.indices
              c1 ← area(r1).indices
              if area(r1)(c1) == '#'
            } rest=rest.update(row + r1,col + c1, '0')

            Paint(row + halfLength, col + halfLength, halfLength) +: erases
          } else IndexedSeq.empty[Command]
        }
        val cmds = commands.flatten
        val (paints, erases) = cmds.partition { case _: Paint => true; case _ => false }
        println(s"${paints.size} paints and ${erases.size} erases for size $halfLength")
        cmds ++ (
          if (full) paintArea(halfLength, stop, !full)
          else paintArea(halfLength - 1, stop, !full))
      }
    }

    Solution(paintArea(25, 0, true))
  }

}