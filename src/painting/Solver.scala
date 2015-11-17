package painting

import painting.Problem.{picture, nrow, ncol}

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 17/11/15
 * Time: 12:36
 */
object Solver {
  def solve = paintArea(5)

  def paintArea(areaSize: Int): IndexedSeq[Command] = {
    if (areaSize <= 0) IndexedSeq.empty[Command]
    else {
      val r = for {
        row ← 0 until nrow by areaSize
        col ← 0 until ncol by areaSize
      } yield {
        val area = picture.slice(row, row + areaSize) map (_.slice(col, col + areaSize))

        if (area.map(_.count('#'.==)).sum > area.map(_.count('.'.==)).sum && area.length == area.head.length) {
          val erases = for {r1 ← area.indices
                            c1 ← area(r1).indices
                            if area(r1)(c1) == '.'
          } yield Erase(r1, c1)
          for {r1 ← row until row + area.length
               c1 ← col until col + area.head.length}
            picture(r1) = picture(r1).updated(c1, '.')
          Paint(row + area.length/2, col + area.length/2 , area.length/2) +: erases
        } else IndexedSeq.empty[Command]
      }
      r.flatten ++ paintArea(areaSize - 2)
    }
  }
}