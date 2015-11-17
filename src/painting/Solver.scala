package painting

import painting.Problem.{picture, nrow, ncol}

/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 17/11/15
 * Time: 12:36
 */
object Solver {
  def solve = {
    val areaSize = 5
    val r = for {
      row ← 0 until nrow by areaSize
      col ← 0 until ncol by areaSize
    } yield {
        val area = picture.slice(row, row + areaSize) map (_.slice(col, col+areaSize))

        if (area.map(_.count('#'.==)).sum > area.map(_.count('.'.==)).sum && area.length == area.head.length) {
          val erases = for { r1 ← area.indices
                             c1 ← area(r1).indices
                             if area(r1)(c1) == '.'
          } yield Erase(r1, c1)
          Paint(row,col,area.length - 1) +: erases
        } else {
          for {
            r1 ← area.indices
            c1 ← area(r1).indices
            if area(r1)(c1) == '#'
          } yield Paint(r1,c1,0)
        }
      }
    r.flatten.toList
  }
}
