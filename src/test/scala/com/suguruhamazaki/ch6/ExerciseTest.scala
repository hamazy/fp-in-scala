package com.suguruhamazaki.ch6

import org.scalatest.{ FlatSpec, Matchers }

class ExerciseTest extends FlatSpec with Matchers {

  import Exercise._
  import Machine._

  "The machine state" should "not be changed by inserting coins when candies are out of stock" in {
    val ((candies, coins), machine) =
      onCoin.run(Machine(true, 0 /*out of stock*/ , 2))
    candies should be(0)
    coins should be(2)
    machine should be(Machine(true, 0, 2))
  }

  it should "not be changed by turning the knob when candies are out of stock" in {
    val ((candies, coins), machine) =
      onTurn.run(Machine(false, 0 /*out of stock*/ , 2))
    candies should be(0)
    coins should be(2)
    machine should be(Machine(false, 0, 2))
  }

  it should "not be changed by turning the knob when the machine is locked" in {
    val ((candies, coins), machine) =
      onTurn.run(Machine(true, 1, 2))
    candies should be(1)
    coins should be(2)
    machine should be(Machine(true, 1, 2))
  }

  it should "not be changed by inserting coins when the machine is already unlocked" in {
    val ((candies, coins), machine) =
      onCoin.run(Machine(false, 1, 2))
    candies should be(1)
    coins should be(2)
    machine should be(Machine(false, 1, 2))
  }

  it should "dispense candy and be locked by turning the knob when it's unlocked" in {
    val ((candies, coins), machine) =
      onTurn.run(Machine(false, 1, 2))
    candies should be(0)
    coins should be(2)
    machine should be(Machine(true, 0, 2))
  }

  it should "be unlocked by inserting a coin when it contains candies" in {
    val ((candies, coins), machine) =
      onCoin.run(Machine(true, 1, 2))
    candies should be(1)
    coins should be(3)
    machine should be(Machine(false, 1, 3))
  }

  it should "not be changed by empty actions" in {
    val ((candies, coins), machine) =
      simulateMachine(List()).run(Machine(true, 1, 2))
    candies should be(1)
    coins should be(2)
    machine should be(Machine(true, 1, 2))
  }

  it should "transit its state by successive actions" in {
    val ((candies, coins), machine) =
      simulateMachine(List(Turn, Coin, Turn, Coin)).run(Machine(true, 3, 2))
    candies should be(1)
    coins should be(4)
    machine should be(Machine(true, 1, 4))
  }
}
