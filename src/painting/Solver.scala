package painting
/**
 * Created with IntelliJ IDEA.
 * User: a203673
 * Date: 17/11/15
 * Time: 12:36
 */
object Solver {
  def solve = {
    val r = for {
      row ← 0 until Problem.nrow
      col ← 0 until Problem.ncol
      if Problem.picture(row)(col) == '#'
    } yield Paint(row,col,1)
    r.toList
  }
}
