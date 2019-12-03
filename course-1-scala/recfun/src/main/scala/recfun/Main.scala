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
  private def customFact(number: Int, limit: Int) = {
    def innerFact(num: Int, res: Int): Int = {
      num match {
        case x if x < limit => res
        case x => innerFact(x-1, res * x)
      }
    }
    innerFact(number, 1)
  }

    def pascal(c: Int, r: Int): Int = {
      customFact(r, r-c+1)/customFact(c, 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def innerBalance(list: List[Char], remainder: Int): Int = {
        if(remainder<0) {
          remainder
        }
        else{
          list match {
            case Nil => remainder
            case head::tail if head == '('=> innerBalance(tail, remainder+1)
            case head::tail if head == ')'=> innerBalance(tail, remainder-1)
            case _::tail => innerBalance(tail, remainder)
          }
        }
      }
      innerBalance(chars, 0) match {
        case x: Int if x == 0 => true
        case x: Int if x != 0 => false
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if(money > 0 && coins.nonEmpty)
        countChange(money-coins.head,coins) + countChange(money,coins.tail)
      else
        0
    }

}
