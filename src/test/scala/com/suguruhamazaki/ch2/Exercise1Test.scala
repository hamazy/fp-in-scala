package com.suguruhamazaki.ch2

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.Matchers

class Exercise1Test extends FlatSpec with BeforeAndAfter with GivenWhenThen with Matchers {

  "fib function" should "produce a Fibonacci number" in {

    Exercise1.fib(1) should equal (0)
    Exercise1.fib(2) should equal (1)

    for (i <- 3 to 10) {
      val prevprev = Exercise1.fib(i - 2)
      val prev = Exercise1.fib(i - 1)
      val expected = prev + prevprev
      Exercise1.fib(i) should equal (expected)
    }
  }
}
