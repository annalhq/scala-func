package recfun

import scala.collection.mutable.{Stack, Map}

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def _pascal(c: Int, r: Int, memo: Map[(Int, Int), Int] = Map()): Int = {
      if (memo.contains((c, r))) {
        memo((c, r))
      } else {
        if (c > r) {
          throw new java.lang.IllegalArgumentException("c cannot be > r.")
        } else if (c == 0 && r == 0) {
          1
        } else if (c == 0 || c == r) {
          1
        } else {
          val resLeft = memo.getOrElse((c - 1, r - 1), default=_pascal(c - 1, r - 1, memo))
          memo((c - 1, r - 1)) = resLeft

          val resRight = memo.getOrElse((c, r - 1), default=_pascal(c, r - 1, memo))
          memo((c, r - 1)) = resRight

          memo((c, r)) = resLeft + resRight
          resLeft + resRight
        }
      }

    }
    _pascal(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    val openClosedMap = Map(('(', ')'))
    val openBrackets: Set[Char] = Set.from(openClosedMap.keys)
    val closedBrackets: Set[Char] = Set.from(openClosedMap.values)

    def _is_balanced(charList: List[Char], stack: Stack[Char]): Boolean = {
      if (charList.isEmpty) {
        if (stack.isEmpty) {
          return true
        } else {
          return false
        }
      }

      val (next, remaining) = (charList.head, charList.tail)

      if (openBrackets.contains(next)) {
        stack.push(next)
      } else if (closedBrackets.contains(next)) {
        if (stack.isEmpty) {
          return false
        }

        val last = stack.pop()
        val complement = openClosedMap(last)
        if (complement != next) {
          return false
        }
      }
      return _is_balanced(remaining, stack)
    }
    return _is_balanced(chars, new Stack[Char]())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {


    def _countChange(m: Int, c: List[Int], memo: Map[(Int, List[Int]), Int] = Map()): Int = {
      if (memo.contains((m, c))) {
        memo((m, c))
      } else {

        if (m == 0) {
          1
        } else if (c.isEmpty || m < 0) {
          0
        } else {
          val withCoinCount = memo.getOrElse((m - c.head, c), default = _countChange(m - c.head, c, memo))
          memo((m - c.head, c) ) = withCoinCount

          val withoutCoinCount = memo.getOrElse((m, c.tail), default = _countChange(m, c.tail, memo))
          memo((m, c.tail)) = withoutCoinCount

          memo((m, c)) = withCoinCount + withoutCoinCount
          withCoinCount + withoutCoinCount
        }
      }
    }
    _countChange(money, coins)
  }
