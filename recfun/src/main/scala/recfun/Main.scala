package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (r < 0)
      0
    else if (c < 0)
      0
    else if (c > r)
      0
    else if (c == 0)
      1
    else if (c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c + 0, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(cs: List[Char], counter: Int): Int = {
      if (counter < 0 ) counter
      else
        cs match {
          case Nil       => counter
          case '(' :: xs => balanceIter(xs, counter + 1)
          case ')' :: xs => balanceIter(xs, counter - 1)
          case  _  :: xs => balanceIter(xs, counter)
        }
    }

    balanceIter(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <  0)         0
    else if (money == 0)    1
    else if (coins.isEmpty) 0
    else countChange(money - coins.head, coins) +
         countChange(money, coins.tail)
  }
}
