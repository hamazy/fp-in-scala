package com.suguruhamazaki.ch2

object Exercise1 {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(count: Int, previous: (Int,Int)): Int = {
      if (count < n) go(count + 1, (previous._1 + previous._2, previous._1))
      else previous._1 + previous._2
    }
    if (n < 0) throw new IllegalArgumentException("Not a natural number.")
    else if (n == 1) 0
    else if (n == 2) 1
    else go(3, (1, 0))
  }
}
