package com.suguruhamazaki.ch6

trait RNG {
  def nextInt: (Int, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFFFFL
    val nextRNG = Simple(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Exercise {
  def positiveInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    val mod = v % Int.MaxValue
    val r =
      if (mod > 0) mod
      else -mod
    (r + 1, rng2)
  }
  def double(rng: RNG): (Double, RNG) = {
    val (v, rng2) = rng.nextInt
    val min = Int.MinValue.toDouble
    val max = Int.MaxValue.toDouble
    ((v.toDouble - min) / (max - min), rng2)
  }
  type Rand[+A] = RNG ⇒ (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] =
    rng ⇒ (a, rng)
  def map[A,B](s: Rand[A])(f: A ⇒ B): Rand[B] =
    rng ⇒ {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  val double2: Rand[Double] =
    map(positiveInt)(i ⇒ (i - 1).toDouble / Int.MaxValue.toDouble)
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) ⇒ C): Rand[C] =
    rng ⇒ {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng)
      (f(a,b), rng3)
    }
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_,_))
  val randIntDouble: Rand[(Int,Double)] =
    both(int, double)
  val doubleInt: Rand[(Double,Int)] =
    both(double, int)
}

