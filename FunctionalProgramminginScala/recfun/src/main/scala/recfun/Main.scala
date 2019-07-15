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
      var cur = 0
      var last = new Array[Int](1)
      last(0) = 1
      while(cur < r) {
        cur += 1
        val temp = new Array[Int](cur + 1)
        for(i <- 0 to cur) {
          if(i == 0 || i == cur) temp(i) = 1
          else temp(i) = last(i - 1) + last(i)
        }
        last = temp
      }
      return last(c)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def helper(cur: List[Char], curNum: Int) : Boolean = cur match {
        case Nil => curNum == 0
        case '(' :: remain => helper(remain, curNum + 1)
        case ')' :: remain =>
          if(curNum - 1 < 0) false
          else helper(remain, curNum - 1)
        case _ :: remain => helper(remain, curNum)
      }
      helper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty || (money <= 0 && !coins.isEmpty)) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
