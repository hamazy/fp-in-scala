package com.suguruhamazaki.ch2

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class Exercise2Test extends FlatSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  "isSorted function (passed a sorted Array)" should "return true" in {

    Given("a sorted array of integers")
    val sortedInts = Array(1, 2, 3, 4)
    val result = Exercise2.isSorted(sortedInts, (a:Int, b:Int) => a < b)
    result should be (true)
  }

  "isSorted function (passed an Array that is not sorted)" should "return true" in {

    Given("an array of integers that is not sorted")
    val notSortedInt = Array(1, 5, 3, 4)
    val result = Exercise2.isSorted(notSortedInt, (a:Int, b:Int) => a < b)
    result should be (false)
  }

}
