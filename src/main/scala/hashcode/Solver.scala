package hashcode

object Solver {
  def solve(problem: Problem): Solution = {
    import problem._

    def paintArea(halfLength: Int, stop: Int, notFull: Boolean): IndexedSeq[Command] = {
      val length = 2 * halfLength + 1
      def shouldPaint(area: Array[String]) =
        if (notFull)
          area.map(_.count('#'.==)).sum > area.map(_.count('.'.==)).sum
        else
          area.forall(_.forall('#'.==))

      if (length <= stop) IndexedSeq.empty[Command]
      else {
        val r = for {
          row ← 0 until nrow by length
          col ← 0 until ncol by length
        } yield {
          val area = picture.slice(row, row + length).map(_.slice(col, col + length))

          if (shouldPaint(area) && area.length == area.head.length) {
            val erases = for {
              r1 ← area.indices
              c1 ← area(r1).indices
              if area(r1)(c1) == '.'
            } yield Erase(row + r1, col + c1)

            for {
              r1 ← area.indices
              c1 ← area(r1).indices
              if area(r1)(c1) == '#'
            } picture(row + r1) = picture(row + r1).updated(col + c1, '0')

            Paint(row + halfLength, col + halfLength, halfLength) +: erases
          } else IndexedSeq.empty[Command]
        }
        r.flatten ++ paintArea(halfLength - 1, stop, notFull)
      }
    }

    Solution(paintArea(25, 3, false) ++ paintArea(2, 0, true))
  }

}