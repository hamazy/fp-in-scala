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

}
