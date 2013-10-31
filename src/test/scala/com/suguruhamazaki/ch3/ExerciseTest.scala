package com.suguruhamazaki.ch3

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.Matchers

class ExerciseTest extends FlatSpec with BeforeAndAfter with GivenWhenThen with Matchers {

  // val list = Cons(1, Cons(2, Cons(3, Nil)))
  val list = List(1, 2, 3)

  "tail" should "remove the first element" in {

    val tail = List.tail(list)
    tail should equal (List(2, 3))
  }

  "drop" should "remove the first n elements" in {

    val dropped1 = List.drop(list, 1)
    dropped1 should equal (List(2, 3))

    val dropped2 = List.drop(list, 2)
    dropped2 should equal (List(3))

    val dropped3 = List.drop(list, 3)
    dropped3 should equal (List())
  }

  "dropWhile" should "remove elements that match the passed f" in {

    val dropped1 = List.dropWhile(list)(_ < 3)
    dropped1 should equal (List(3))

    val dropped2 = List.dropWhile(list)(_ % 2 == 1)
    dropped2 should equal (List(2, 3))
  }

  "setHead" should "replace the first element in the list" in {

    val replaced1 = List.setHead(list, 5)
    replaced1 should equal (List(5, 2, 3))
  }

  "init" should "return a list which has all but the last element" in {

    val result = List.init(list)
    result should equal (List(1, 2))
  }

  "length" should "return the length of a list" in {

    val result = List.length(list)
    result should equal (3)

    val emptyLength = List.length(Nil)
    emptyLength should equal (0)
  }

  "foldLeft" should "compute a sum" in {

    val result = List.foldLeft(list, 0.0)(_ + _)
    result should equal (1 + 2 + 3)
  }

  "foldLeft" should "compute a product" in {

    val result = List.foldLeft(list, 1.0)(_ * _)
    result should equal (1 * 2 * 3)
  }

  "sumByFoldLeft" should "compute a sum" in {

    val result = List.sumByFoldLeft(list)
    result should equal (1 + 2 + 3)
  }

  "productByFoldLeft" should "compute a product" in {

    val result = List.productByFoldLeft(List(1.0, 2.0, 3.0))
    result should equal (1 * 2 * 3)
  }

  "reverse" should "reverse the list" in {

    val result = List.reverse(list)
    result should equal (List(3, 2, 1))
  }

  "append" should "append an element to a list" in {

    val result = List.append(list, 4)
    result should equal (List(1, 2, 3, 4))

    val result2 = List.append(result, 5)
    result2 should equal (List(1, 2, 3, 4, 5))
  }

  "concat" should "concatenate two lists" in {

    val result = List.concat(list, List(4, 5, 6))
    result should equal (List(1, 2, 3, 4, 5, 6))
  }

  "add1" should "add one to each element of a list" in {

    List.add1(list) should equal (List(2, 3, 4))
  }

  "stringify" should "make each element of a list a string" in {

    List.stringify(List(1.0, 2.0, 3.0)) should equal (List("1.0", "2.0", "3.0"))
  }

  "map" should "transform each element of a list with a function" in {

    List.map(list)(_ + 1) should equal (List(2, 3, 4))
    List.map(List(1.0, 2.0, 3.0))(_.toString) should equal (List("1.0", "2.0", "3.0"))
  }

  "filter" should "return a list where elements that don't satisfy the given predicate are removed" in {

    List.filter(list)(_ % 2 == 0) should equal (List(2))
  }
}
