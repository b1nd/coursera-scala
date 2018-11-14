package recfun

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
  def pascal(c: Int, r: Int): Int = (c, r) match {
    case (0, _) => 1
    case (`c`, `r`) => if (c == r) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char], count: Int = 0): Boolean = (chars, count) match {
    case (cs, c) => if (cs.isEmpty) c == 0 else cs.head match {
      case '(' => balance(cs.tail, c + 1)
      case ')' => if (c > 0) balance(cs.tail, c - 1) else false
      case _ => balance(cs.tail, c)
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (m, cs) => if (m < 0 || cs.isEmpty) 0 else countChange(m - cs.head, cs) + countChange(m, cs.tail)
  }
}
