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
  // ex2
  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1.flatMap(_ => a2) // a2.flatMap(_ => a1)
    def zero: Option[A] = None
  }
  // ex3
  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1.compose(a2)
    def zero: A => A = a => a
  }
}

trait Foldable[F[_]] {
  // ex6
  // in terms of foldMap
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap(as)(f.curried)(Monoid.endoMonoid)(z)

  // ex6
  // in terms of foldMap
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap(as)({ (a: A, b: B) => f(b, a) }.curried)(Monoid.endoMonoid)(z)
  // ex5
  // in terms of foldRight or foldLeft
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero) { (a, b) => mb.op(f(a), b) }

}
