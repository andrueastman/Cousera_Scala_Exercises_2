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
      if(c==r || c==0) 1
      else pascal(c,r-1)+pascal(c-1,r-1)
    }
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanced(chars: List[Char], open: Int): Boolean = {
          if (chars.isEmpty) open == 0      //return if the expression is balance if at end of list
          else if (chars.head == '(') balanced(chars.tail,open+1)  //opening bracket found      
          else if (chars.head == ')') open>0 && balanced(chars.tail,open-1) //closing bracket found(if negative fails)
          else balanced(chars.tail,open)//move forward
      }      
      balanced(chars,0)
  }
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {//credit to stack overflow :)
      if(money == 0)1
      else if(money > 0 && !coins.isEmpty)
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  }
