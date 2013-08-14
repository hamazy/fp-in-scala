package com.suguruhamazaki.ch2

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class Exercise5Test extends FlatSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  "uncurry function" should "return a uncurried function" in {

    case class A(value: Int)
    case class B(value: Int)
    case class C(value: Int)

    val sum = (a: A) => (b: B) => C(a.value + b.value)
    val func1 = Exercise5.uncurry(sum)
    val actual = func1(A(1), B(2))

    val expected = sum(A(1))(B(2))
    actual should equal (expected)
  }
}
