package com.suguruhamazaki.ch4

import org.scalatest.{BeforeAndAfter, FlatSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers

class ExerciseTest extends FlatSpec with BeforeAndAfter with GivenWhenThen with ShouldMatchers {

  "Some" should "be mapped to Some" in {

    val result = Some(1).map(_ + 1)
    result should be (Some(2))
  }

  "None" should "be mapped to None" in {

    val opt: Option[Int] = None
    val r = opt.map(_ + 1)
    r should be (None)
  }

  "Some" should "be flatMap-ped" in {

    Some(1).flatMap(n => Some(n + 1)) should be (Some(2))
    Some(1).flatMap(n => None) should be (None)
  }

  "None" should "be flatMap-ped" in {

    val opt:Option[Int] = None
    opt.flatMap(n => Some(n + 1)) should be (None)
    opt.flatMap(n => None) should be (None)
  }

  "getOrElse applied to Some" should "return the value" in {

    Some(1).getOrElse(fail()) should be (1)
  }

  "getOrElse applied to None" should "return the default value" in {

    val opt:Option[Int] = None
    opt.getOrElse(1) should be (1)
  }

  "orElse applied to Some" should "return a Some of the value" in {

    Some(1).orElse(fail()) should be (Some(1))
  }

  "orElse applied to None" should "return a Some of the default value" in {

    val opt:Option[Int] = None
    opt.orElse(Some(1)) should be (Some(1))
    opt.orElse(None) should be (None)
  }

  "filter (match, applied to Some)" should "return Some" in {

    Some(1).filter(_ % 2 == 1) should be (Some(1))
  }

  "filter (unmatch, applied to Some)" should "return None" in {

    Some(1).filter(_ % 2 == 0) should be (None)
  }

  "filter (applied to None)" should "return None" in {

    val opt:Option[Int] = None
    opt.filter(_ % 2 == 1) should be (None)
  }

  "variance" should "calculate variance" in {

    Exercise2.variance(List(1,1,1)) should be (Some(0.toDouble))
    Exercise2.variance(List(1,2,1,2)) should be (Some(0.25.toDouble))
    Exercise2.variance(List(1,1,2,4)) should be (Some(1.5.toDouble))
  }

  "map2" should "return an Option of C" in {

    Exercise3.map2(Some(1), Some(2)) {
      (a: Int, b: Int) ⇒ a + b
    } should be (Some(3))

    Exercise3.map2(Some(1), None) {
      (a: Int, b: Int) ⇒ a + b
    } should be (None)

    Exercise3.map2(None, Some(2)) {
      (a: Int, b: Int) ⇒ a + b
    } should be (None)
  }

  "bothMatch" should "return an Option of Boolean" in {

    Exercise4.bothMatch_2("a+", "[a-z]+", "a") should be (Some(true))
    Exercise4.bothMatch_2("a+", "[a-z]+", "b") should be (Some(false))
    Exercise4.bothMatch_2("a+", "[a-z", "a") should be (None)
    Exercise4.bothMatch_2("a\\", "[a-z]", "a") should be (None)
  }

  "sequence" should "return an Option of List of A" in {

    Exercise5.sequence(List(Some(1), Some(2), Some(3))) should be (Some(List(1, 2, 3)))
    Exercise5.sequence(List(None, Some(2), Some(3))) should be (None)
    Exercise5.sequence(List(Some(1), None, Some(3))) should be (None)
    Exercise5.sequence(List(Some(1), Some(2), None)) should be (None)
  }

  val toInt: (String) ⇒ Option[Int] = s ⇒ try {
    Some(Integer.parseInt(s))
  } catch {
    case _: Exception ⇒ None
  }
  "traverse" should "return an Option of List of B"  in {

    Exercise6.traverse(List("1", "2", "3"))(toInt) should be (Some(List(1, 2, 3)))
    Exercise6.traverse(List("foo", "2", "3"))(toInt) should be (None)
    Exercise6.traverse(List("1", "foo", "3"))(toInt) should be (None)
    Exercise6.traverse(List("1", "2", "foo"))(toInt) should be (None)
  }

  import Exercise7.{Either, Left, Right}
  "map" should "keep a Left as it is" in {
    Left("foo").map((i: Int) ⇒ i.toFloat) should be (Left("foo"))
  }

  "map" should "convert a Right applying the given function" in {
    Right(123).map(_.toFloat) should be (Right(123.0))
  }

  "flatMap" should "keep a Left as it is" in {
    Left("foo").flatMap((i: Int) ⇒ Left("bar")) should be (Left("foo"))
    Left("foo").flatMap((i: Int) ⇒ Right((i.toFloat))) should be (Left("foo"))
  }

  "flatMap" should "return a Left" in {
    Right(123).flatMap((i: Int) ⇒ Left("bar")) should be (Left("bar"))
  }

  "flatMap" should "return a Right" in {
    Right(123).flatMap((i: Int) ⇒ Right((i.toFloat))) should be (Right(123.0))
  }

  "orElse" should "ruturn the alternative" in {
    Left("foo").orElse(Left(123)) should be (Left(123))
    Left("foo").orElse(Right(123)) should be (Right(123))
  }

  "orElse" should "keep the original" in {
    Right(123).orElse(Left("foo")) should be (Right(123))
    Right(123).orElse(Right(4.56)) should be (Right(123))
  }

  "map2" should "keep a Left as it is" in {
    Left("foo").map2(Left(123)) { (a, b) ⇒  } should be (Left("foo"))
    Left("foo").map2(Right(123)) { (a, b) ⇒  } should be (Left("foo"))
  }

  "map2" should "convert a Right applying the given function" in {
    Right(123).map2(Left("foo")) { (a, b) ⇒  } should be (Left("foo"))
    Right(123).map2(Right(456)) { (a, b) ⇒ a + b } should be (Right(579))
  }

  "sequence" should "return an Either " in {
    Exercise8.sequence(List(Right(1), Right(2), Right(3))) should be (Right(List(1, 2, 3)))
    Exercise8.sequence(List(Left("foo"), Right(2), Right(3))) should be (Left("foo"))
    Exercise8.sequence(List(Right(1), Left("foo"), Right(3))) should be (Left("foo"))
    Exercise8.sequence(List(Right(1), Right(2), Left("foo"))) should be (Left("foo"))
  }

  "traverse" should "return an Either" in {
    val f = (s: String) ⇒ toInt(s).map(Right(_)).getOrElse(Left("parse failed"))

    Exercise8.traverse(List(Right("1"), Right("2"), Right("3")))(f) should be (Right(List(1, 2, 3)))
    Exercise8.traverse(List(Right("foo"), Right("2"), Right("3")))(f) should be (Left("parse failed"))
    Exercise8.traverse(List(Right("1"), Right("foo"), Right("3")))(f) should be (Left("parse failed"))
    Exercise8.traverse(List(Right("1"), Right("2"), Right("foo")))(f) should be (Left("parse failed"))

    Exercise8.traverse(List(Left("foo"), Right("2"), Right("3")))(f) should be (Left("foo"))
    Exercise8.traverse(List(Right("1"), Left("foo"), Right("3")))(f) should be (Left("foo"))
    Exercise8.traverse(List(Right("1"), Right("2"), Left("foo")))(f) should be (Left("foo"))
  }
}
