package hashcode

case class Problem(picture: Array[String], nrow: Int, ncol: Int) {
  def createCopy: Problem = copy(picture = picture.clone())
}
