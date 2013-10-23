package com.suguruhamazaki.ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A => B): Option[B] =
    Some(f(get))

  def flatMap[B](f: A => Option[B]): Option[B] =
    f(get)

  def getOrElse[B >: A](default: => B): B =
    get

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this

  def filter(f: A => Boolean): Option[A] =
    if (f(get)) this
    else None
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing => B): Option[B] =
    None

  def flatMap[B](f: Nothing => Option[B]): Option[B] =
    None

  def getOrElse[B >: Nothing](default: => B): B =
    default

  def orElse[B >: Nothing](ob: => Option[B]): Option[B] =
    ob

  def filter(f: Nothing => Boolean): Option[Nothing] =
    None

}

object Exercise2 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m =>
      mean(xs.map { x =>
        math.pow(x - m, 2)
      })
    }

}

object Exercise3 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] = for {
    x ← a
    y ← b
  } yield f(x, y)
}

object Exercise4 {
  import java.util.regex.{Pattern, PatternSyntaxException}
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException ⇒ None
    }
  def mkMatcher(pat: String): Option[String ⇒ Boolean] =
    pattern(pat) map ( p ⇒ (s: String) ⇒ p.matcher(s).matches)
  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    Exercise3.map2(mkMatcher(pat1), mkMatcher(pat2)) { (f, g) ⇒ f(s) && g(s) }
}

object Exercise5 {
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case x :: Nil ⇒ x.map( _::Nil)
      case x :: xs ⇒ {
        x flatMap { xx ⇒
          sequence(xs) map { xxs ⇒
            xx::xxs
          }
        }
      }
    }
}
