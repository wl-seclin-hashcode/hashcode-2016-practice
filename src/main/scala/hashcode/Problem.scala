package hashcode

case class Problem(picture: Vector[String], nrow: Int, ncol: Int) {
  def update(row: Int, col: Int, c: Char) =
    copy(picture = picture.updated(row, picture(row).updated(col, c)))
}

