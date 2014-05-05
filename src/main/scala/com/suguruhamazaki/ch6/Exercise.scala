package com.suguruhamazaki.ch6

import scala.annotation.tailrec

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

  // ex1 uniform random integer on [0, Int.MaxValue)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    val mod = v % Int.MaxValue
    (mod.abs, rng2)
  }

  // ex2 uniform random double number on [0, 1)
  def double(rng: RNG): (Double, RNG) = {
    val (v, rng2) = rng.nextInt
    val min = Int.MinValue
    val max = Int.MaxValue
    ((v - 1 - min).toDouble / (max - min), rng2)
  }

  // ex3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }
  // ex3
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }
  // ex3
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // ex4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    count match {
      case 0 ⇒ (List(), rng)
      case _ ⇒ {
        val (i, rng2) = rng.nextInt
        val (list, rng3) = ints(count - 1)(rng2)
        (i :: list, rng3)
      }
    }
  }
  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def recurse(count: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
      count match {
        case 0 ⇒ (list, rng)
        case _ ⇒ {
          val (i, rng2) = rng.nextInt
          recurse(count - 1, rng2, i :: list)
        }
      }
    }
    recurse(count, rng, List())
  }

  type Rand[+A] = RNG ⇒ (A, RNG)
  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng ⇒ (a, rng)
  def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] = { rng ⇒
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i ⇒ i - i % 2)

  // ex5
  val double2: Rand[Double] =
    map(int) { i ⇒
      val min = Int.MinValue
      val max = Int.MaxValue
      (i - 1 - min).toDouble / (max - min)
    }

  // ex6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] = { rng ⇒
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))
  val randIntDouble: Rand[(Int, Double)] =
    both(int, double2)
  val doubleInt: Rand[(Double, Int)] =
    both(double2, int)

  // ex7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = { rng ⇒
    fs match {
      case Nil ⇒ (List(), rng)
      case head :: rest ⇒ {
        val (a, rng2) = head(rng)
        val (b, rng3) = sequence(rest)(rng2)
        (a :: b, rng3)
      }
    }
  }
  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @tailrec
    def recurse(fs: List[Rand[A]], list: List[A], rng: RNG): (List[A], RNG) =
      fs match {
        case Nil ⇒ (list, rng)
        case head :: rest ⇒ {
          val (a, rng2) = head(rng)
          recurse(rest, a :: list, rng2)
        }
      }
    { rng ⇒
      recurse(fs, List(), rng)
    }
  }
  def ints3(count: Int)(rng: RNG): (List[Int], RNG) = {
    val ints: List[Rand[Int]] = List.fill(count)(int)
    sequence2(ints)(rng)
  }

  // ex8
  def flatMap[A, B](f: Rand[A])(g: A ⇒ Rand[B]): Rand[B] = { rng ⇒
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i ⇒
      val mod = i % n
      if (i + (n - 1) - mod >= 0) { rng ⇒ (mod, rng) }
      else nonNegativeLessThan(mod)
    }

  // ex9
  object ex9 {
    def map[A, B](s: Rand[A])(f: A ⇒ B): Rand[B] =
      flatMap(s) { a ⇒ rng ⇒ (f(a), rng) }
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) ⇒ C): Rand[C] =
      flatMap(ra) { a ⇒
        map(rb) { b ⇒
          f(a, b)
        }
      }
  }
}

// ex10
case class State[S, +A](run: S ⇒ (A, S)) {
  def map[B](f: A ⇒ B): State[S, B] =
    State { s ⇒
      val (a, s2) = run(s)
      (f(a), s2)
    }
  def map2[B, C](sb: State[S, B])(f: (A, B) ⇒ C): State[S, C] =
    flatMap { a ⇒
      sb.map { b ⇒
        f(a, b)
      }
    }
  def flatMap[B](f: A ⇒ State[S, B]): State[S, B] =
    State { s ⇒
      val (a, s2) = run(s)
      f(a).run(s2)
    }
}

object State {
  def unit[S, A](a: A): State[S, A] = State { (a, _) }
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
  def get[S]: State[S, S] = State(s ⇒ (s, s))
  def set[S](s: S): State[S, Unit] =
    State { _ ⇒
      ((), s)
    }
}

object RandRewritten {
  type Rand[A] = State[RNG, A]
  val int: Rand[Int] = State(_.nextInt)
  def ints(x: Int): Rand[List[Int]] = State { s ⇒
    Exercise.ints3(x)(s)
  }
  val ns: Rand[List[Int]] =
    int.flatMap { x ⇒
      int.flatMap { y ⇒
        ints(x).map { xs ⇒
          xs.map(_ % y)
        }
      }
    }
  val ns2: Rand[List[Int]] =
    for {
      x ← int
      y ← int
      xs ← ints(x)
    } yield xs.map(_ % y)
}

sealed trait Input
// insert a coin
case object Coin extends Input
// turn the knob to dispense candy
case object Turn extends Input

case class Machine(
  locked: Boolean,
  candies: Int,
  coins: Int)

object Machine {

  val onCoin: State[Machine, (Int, Int)] =
    State {
      case m @ Machine(_, 0, _) ⇒ ((0, m.coins), m)
      case m @ Machine(true, _, _) ⇒ {
        val coins = m.coins + 1
        ((m.candies, coins), Machine(false, m.candies, coins))
      }
      case m @ Machine(false, _, _) ⇒ ((m.candies, m.coins), m)
    }
  val onTurn: State[Machine, (Int, Int)] =
    State {
      case m @ Machine(true, _, _) ⇒ ((m.candies, m.coins), m)
      case m @ Machine(_, 0, _) ⇒ ((m.candies, m.coins), m)
      case m @ Machine(_, _, _) ⇒ {
        val candies = m.candies - 1
        ((candies, m.coins), Machine(true, candies, m.coins))
      }
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs match {
      case Nil ⇒ State { m ⇒ ((m.candies, m.coins), m) }
      case Coin :: rest ⇒ {
        val state = simulateMachine(rest)
        state.flatMap(_ ⇒ onCoin)
      }
      case Turn :: rest ⇒ {
        val state = simulateMachine(rest)
        state.flatMap(_ ⇒ onTurn)
      }
    }
}
