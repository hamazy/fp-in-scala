package com.suguruhamazaki.ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil ⇒ Nil
    case Cons(h, t) ⇒ t
  }

  // Exercise 3
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n > 0) drop(tail(l), n - 1)
    else if (n == 0) l
    else throw new IllegalArgumentException

  // Exercise 4
  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A ⇒ Boolean): List[A] = l match {
    case Cons(h, t) if f(h) ⇒ dropWhile(t)(f)
    case other ⇒ other
  }

  // Exercise 5
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(h, t) ⇒ Cons(a, t)
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] = l match {
    case Nil ⇒ Nil
    case Cons(head, Nil) ⇒ Nil
    case Cons(head, Cons(last, Nil)) ⇒ Cons(head, Nil)
    case Cons(head, tail) ⇒ Cons(head, init(tail)) // not tail-recursive
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) ⇒ B): B =
    l match {
      case Nil ⇒ z
      case Cons(x, xs) ⇒ f(x, foldRight(xs, z)(f)) // not tail-recursive
    }

  def sum(l: List[Int]) =
    foldRight(l, 0.0)(_ + _)

  def product(l: List[Double]) =
    foldRight(l, 1.0)(_ * _)

  // Exercise 9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((a, b) ⇒ 1 + b)

  // Exercise 10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) ⇒ B): B =
    l match {
      case Nil ⇒ z
      case Cons(x, xs) ⇒ foldLeft(xs, f(z, x))(f)
    }

  // Exercise 11
  def sumByFoldLeft(l: List[Int]) =
    foldLeft(l, 0.0)(_ + _)

  // Exercise 11
  def productByFoldLeft(l: List[Double]) =
    foldLeft(l, 1.0)(_ * _)

  // Exercise 12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((xs,x) ⇒ Cons(x,xs))

  // Exercise 14
  def append[A](l: List[A], z: A): List[A] =
    foldRight(l, Cons(z,Nil))(Cons(_,_))

  // Exercise 15
  def concat[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l2, l1)(append)

  // Exercise 16
  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((a, b) ⇒ Cons(a + 1, b))

  // Exercise 17
  def stringify(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((a, b) ⇒ Cons(a.toString, b))

  // Exercise 18
  def map[A,B](l: List[A])(f: A ⇒ B): List[B] =
    foldRight(l, Nil:List[B])((a, b) ⇒ Cons(f(a), b))

  // Exercise 19
  def filter[A](l: List[A])(f: A ⇒ Boolean): List[A] =
    l match {
      case Nil ⇒ Nil
      case Cons(x, xs) if f(x) ⇒ Cons(x , filter(xs)(f)) // not tail-recursive
      case Cons(x, xs) ⇒ filter(xs)(f) // not tail-recursive
    }
}
