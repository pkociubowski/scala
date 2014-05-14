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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)


  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def balWithSum(chars: List[Char], sum: Int): Boolean = {
      if (sum < 0)
        false
      else if (chars.isEmpty)
        !(sum > 0)
      else
        balWithSum(chars.tail, sum + getValueOfChar(chars.head))
    }

    def getValueOfChar(char: Char): Int = {
      if (char == '(')
        1
      else if (char == ')')
        -1
      else
        0
    }
    if (!chars.isEmpty)
      balWithSum(chars, 0)
    else
      true
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def auxiliary(coins: List[Int], sum: Int): Int = {
      if (sum == money)
        1
      else if (sum > money || coins.isEmpty)
        0
      else
        auxiliary(coins, sum + coins.head) + auxiliary(coins.tail, sum)
    }

    if (money <= 0)
      0
    else
      auxiliary(coins, 0)
  }
}
