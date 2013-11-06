package com.suguruhamazaki.ch5

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.Matchers

class ExerciseTest extends FlatSpec with BeforeAndAfter with GivenWhenThen with Matchers {

  "Stream" should "be converted to List" in {
    Stream().toList should be (List())
    Stream(1, 2, 3).toList should be (List(1, 2, 3))
  }

  "take()" should "return a partial Stream from front" in {
    Stream(1, 2, 3).take(0).toList should be (List(1, 2, 3))
    Stream(1, 2, 3).take(1).toList should be (List(1))
    Stream(1, 2, 3).take(2).toList should be (List(1, 2))
    Stream(1, 2, 3).take(3).toList should be (List(1, 2, 3))
    Stream(1, 2, 3).take(4).toList should be (List(1, 2, 3))
  }
}
