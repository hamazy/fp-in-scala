package com.suguruhamazaki.ch10

import scala.language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    def zero: String = ""
  }
  // ex1
  val intAddition = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    def zero: Int = 0
  }
  val intMultiplication = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    def zero: Int = 1
  }
  val booleanOr = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 | a2
    def zero: Boolean = false
  }
  val booleanAnd = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 & a2
    def zero: Boolean = false
  }
}

trait Foldable[F[_]] {
  // ex6
  // in terms of foldMap
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???
  // ex6
  // in terms of foldMap
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???
  // ex5
  // in terms of foldRight or foldLeft
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = ???

}
