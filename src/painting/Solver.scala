package painting

import painting.Problem.{picture, nrow, ncol}

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 17/11/15
 * Time: 12:36
 */
object Solver {
  def solve = { paintArea(51, 6, false) ++ paintArea(5, 0, true) }

  def paintArea(areaSize: Int, stop:Int, notFull: Boolean): IndexedSeq[Command] = {
    def shouldPaint(area: Array[String]) = if (notFull)
      area.map(_.count('#'.==)).sum > area.map(_.count('.'.==)).sum
    else
      area.forall(_.forall('#'.==))

    if (areaSize <= stop ) IndexedSeq.empty[Command]
    else {
      val r = for {
        row ← 0 until nrow by areaSize
        col ← 0 until ncol by areaSize
      } yield {
        val area = picture.slice(row, row + areaSize) map (_.slice(col, col + areaSize))

        if (shouldPaint(area) && area.length == area.head.length) {
          val erases = for {r1 ← area.indices
                            c1 ← area(r1).indices
                            if area(r1)(c1) == '.'
          } yield Erase(row + r1, col + c1)
          for {r1 ← area.indices
               c1 ← area(r1).indices
               if area(r1)(c1) == '#'
          } picture(row + r1) = picture(row + r1).updated(col + c1, '0')
          Paint(row + area.length/2, col + area.length/2 , area.length/2) +: erases
        } else IndexedSeq.empty[Command]
      }
      r.flatten ++ paintArea(areaSize - 2, stop, notFull)
    }
  }
}