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
    def pascal(c: Int, r: Int): Int = {
      if (c ==0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceHelper(nLeft: Int, myList: List[Char]): Boolean = {
        if (nLeft < 0) return false
        if (myList.isEmpty) {
          if (nLeft == 0) return true
          else return false
        }

        if (myList.head == '(') balanceHelper(nLeft + 1, myList.tail)
        else if (myList.head == ')') balanceHelper(nLeft - 1, myList.tail)
        else balanceHelper(nLeft, myList.tail)
      }

      balanceHelper(0, chars)
    }


  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) return 0
      if (money == 0) return 1
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }


