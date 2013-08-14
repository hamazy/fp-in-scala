package com.suguruhamazaki.ch2

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class Exercise6Test extends FlatSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  "compose function" should "return a composed function" in {

    case class A(value: Int)
    case class B(value: Int)
    case class C(value: Int)

    val inc = (a: A) => B(a.value + 1)
    val square = (b: B) => C(b.value * b.value)
    val func = Exercise6.compose(square, inc)
    func(A(1)) should equal (C(4))
  }
}
