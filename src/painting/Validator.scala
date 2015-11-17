package painting

import scala.annotation.tailrec

object Validator {
  def validate(sol: List[Command], picture: Array[String]) {
    val painted = paintRec(sol, Map.empty.withDefaultValue(false))
    for {
      i <- 0 until picture.length
      line = picture(i)
      j <- 0 until line.length
      c = line(j)
      p = c == '#'
    } assert(painted((i, j)) == p, s"($i,$j) should be $p")
  }

  @tailrec
  def paintRec(sol: List[Command], picture: Map[(Int, Int), Boolean]): Map[(Int, Int), Boolean] = sol match {
    case Nil             => picture
    case command :: tail => paintRec(tail, command.update(picture))
  }

}