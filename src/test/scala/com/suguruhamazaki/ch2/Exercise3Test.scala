package com.suguruhamazaki.ch2

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.Matchers

class Exercise3Test extends FlatSpec with BeforeAndAfter with GivenWhenThen with Matchers {

  "partial1 function" should "produce a partially applied function" in {

    case class A(value: Int)
    case class B(value: Int)
    case class C(value: Int)

    val sum = (a: A, b: B) ⇒ C(a.value + b.value)
    val inc1 = Exercise3.partial1(A(1), sum)
    inc1(B(1)) should equal (C(2))
    inc1(B(2)) should equal (C(3))
  }
}
