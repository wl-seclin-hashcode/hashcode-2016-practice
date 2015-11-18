package hashcode

import java.io.File
import java.util.Scanner

object Parser {
  def read(): Problem = {
    val scan = new Scanner(new File("doodle.txt"))
    import scan._
    val nrow = nextInt()
    val ncol = nextInt()
    nextLine()
    val picture = Array.fill(nrow) { nextLine() }

    Problem(picture, nrow, ncol)
  }
}