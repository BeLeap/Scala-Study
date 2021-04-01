package recfun
import common._

object Main {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row) {
        val result = pascal(col, row)
        print(s"$result ")
      }
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (c == 0) 1 else if (r == c) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def innerBalance(chars: List[Char], openedCount: Int): Boolean = {
    chars match {
      case ('('::cs) => innerBalance(cs, openedCount + 1)
      case (')'::cs) => {
        if (openedCount - 1 >= 0) {
          innerBalance(cs, openedCount - 1)
        } else {
          false
        }
      }
      case (_::cs) => innerBalance(cs, openedCount)
      case List() => {
        openedCount match {
          case 0 => true
          case _ => false
        }
      }
    }
  }
  def balance(chars: List[Char]): Boolean = innerBalance(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = { 0 }
}
