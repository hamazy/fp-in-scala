package com.suguruhamazaki.ch4

sealed trait Option[+A] {
  def map[B](f: A ⇒ B): Option[B]
  def flatMap[B](f: A ⇒ Option[B]): Option[B]
  def getOrElse[B >: A](default: ⇒ B): B
  def orElse[B >: A](ob: ⇒ Option[B]): Option[B]
  def filter(f: A ⇒ Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {
  def map[B](f: A ⇒ B): Option[B] =
    Some(f(get))

  def flatMap[B](f: A ⇒ Option[B]): Option[B] =
    f(get)

  def getOrElse[B >: A](default: ⇒ B): B =
    get

  def orElse[B >: A](ob: ⇒ Option[B]): Option[B] =
    this

  def filter(f: A ⇒ Boolean): Option[A] =
    if (f(get)) this
    else None
}

case object None extends Option[Nothing] {
  def map[B](f: Nothing ⇒ B): Option[B] =
    None

  def flatMap[B](f: Nothing ⇒ Option[B]): Option[B] =
    None

  def getOrElse[B >: Nothing](default: ⇒ B): B =
    default

  def orElse[B >: Nothing](ob: ⇒ Option[B]): Option[B] =
    ob

  def filter(f: Nothing ⇒ Boolean): Option[Nothing] =
    None

}

object Exercise2 {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap { m ⇒
      mean(xs.map { x ⇒
        math.pow(x - m, 2)
      })
    }

}

object Exercise3 {
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) ⇒ C): Option[C] = for {
    x ← a
    y ← b
  } yield f(x, y)
}

object Exercise4 {
  import java.util.regex.{ Pattern, PatternSyntaxException }
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException ⇒ None
    }
  def mkMatcher(pat: String): Option[String ⇒ Boolean] =
    pattern(pat) map (p ⇒ (s: String) ⇒ p.matcher(s).matches)
  def bothMatch2(pat1: String, pat2: String, s: String): Option[Boolean] =
    Exercise3.map2(mkMatcher(pat1), mkMatcher(pat2)) { (f, g) ⇒ f(s) && g(s) }
}

object Exercise5 {
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil ⇒ Some(List())
      case elem :: Nil ⇒ elem.map(_ :: Nil)
      case head :: rest ⇒ {
        head flatMap { elem ⇒
          sequence(rest) map { list ⇒
            elem :: list
          }
        }
      }
    }
}

object Exercise6 {
  def traverse[A, B](a: List[A])(f: A ⇒ Option[B]): Option[List[B]] =
    a match {
      case Nil ⇒ Some(List())
      case x :: Nil ⇒ f(x).map(_ :: Nil)
      case x :: xs ⇒ f(x).flatMap { elem ⇒
        traverse(xs)(f) map { list ⇒
          elem :: list
        }
      }
    }
}

object Exercise7 {
  sealed trait Either[+E, +A] {
    def map[B](f: A ⇒ B): Either[E, B]
    def flatMap[EE >: E, B](f: A ⇒ Either[EE, B]): Either[EE, B]
    def orElse[EE >: E, B >: A](b: ⇒ Either[EE, B]): Either[EE, B]
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) ⇒ C): Either[EE, C]
  }
  case class Left[+E](value: E) extends Either[E, Nothing] {
    def map[B](f: Nothing ⇒ B): Either[E, Nothing] = this
    def flatMap[EE >: E, B](f: Nothing ⇒ Either[EE, B]): Either[EE, B] = this
    def orElse[EE >: E, B >: Nothing](b: ⇒ Either[EE, B]): Either[EE, B] = b
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) ⇒ C): Either[EE, C] = this
  }
  case class Right[+A](value: A) extends Either[Nothing, A] {
    def map[B](f: A ⇒ B): Either[Nothing, B] = Right(f(value))
    def flatMap[Nothing, B](f: A ⇒ Either[Nothing, B]): Either[Nothing, B] = f(value)
    def orElse[Nothing, B >: A](b: ⇒ Either[Nothing, B]): Either[Nothing, B] = this
    def map2[Nothing, B, C](b: Either[Nothing, B])(f: (A, B) ⇒ C): Either[Nothing, C] =
      b.map { bb ⇒ f(value, bb) }
  }
}

object Exercise8 {
  import Exercise7.{ Either, Left, Right }
  def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
    a match {
      case Nil ⇒ Right(Nil)
      case elem :: Nil ⇒ elem.map { _ :: Nil }
      case head :: rest ⇒ {
        head flatMap { elem ⇒
          sequence(rest) map { list ⇒
            elem :: list
          }
        }
      }
    }
  def traverse[E, A, B](a: List[Either[E, A]])(f: A ⇒ Either[E, B]): Either[E, List[B]] =
    a match {
      case Nil ⇒ Right(Nil)
      case elem :: Nil ⇒ elem flatMap { v ⇒
        f(v).map(List(_))
      }
      case head :: rest ⇒
        head flatMap { elem ⇒
          traverse(rest)(f) flatMap { list ⇒
            f(elem) map { _ :: list }
          }
        }
    }
}
