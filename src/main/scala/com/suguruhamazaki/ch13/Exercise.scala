package com.suguruhamazaki.ch13

import com.suguruhamazaki.ch12.Monad
import scala.language.higherKinds

package stackcunsuming {

  import scala.io.StdIn

  sealed trait IO[A] { self =>
    def run: A
    def map[B](f: A => B): IO[B] =
      new IO[B] { def run = f(self.run) }
    def flatMap[B](f: A => IO[B]): IO[B] =
      new IO[B] { def run = f(self.run).run }
  }

  object IO extends Monad[IO] {
    def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]) = fa.flatMap(f)
    def apply[A](a: => A): IO[A] = unit(a)
  }

  object Converter {
    def fahrenheitToCelsius(f: Double): Double =
      (f - 32) * 5.0 / 9.0
    def ReadLine: IO[String] = IO { StdIn.readLine }
    def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

    def converter: IO[Unit] = for {
      _ <- PrintLine("Enter a temprature in degree Fahrenheit:")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheitToCelsius(d).toString)
    } yield ()
  }

}

package stackfree {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      FlatMap(this, f)
    def map[B](f: A => B): IO[B] =
      flatMap(f.andThen(Return(_)))
  }

  case class Return[A](a: A) extends IO[A]
  case class Suspend[A](resume: () => A) extends IO[A]
  case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]

}

sealed trait Free[F[_], A] {
  // ex1
  def map[B](f: A => B): Free[F, B] =
    flatMap(f.andThen(Return(_)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap(this, f)
}
case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](s: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type TailRec[A] = Free[Function0, A]

  // ex1
  def freeMonad[F[_]] = new Monad[({ type f[a] = Free[F, a] })#f] {
    def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa.flatMap(f)
  }

  // ex2
  @annotation.tailrec
  def runTranpoline[A](a: Free[Function0, A]): A =
    a match {
      case Return(x) => x
      case Suspend(f) => f()
      case FlatMap(x, f) => x match {
        case Return(y) => runTranpoline(f(y))
        case Suspend(g) => runTranpoline(f(g()))
        case FlatMap(y, g) => runTranpoline(y.flatMap(z => g(z).flatMap(f)))
      }
    }

  // ex3
  def run[F[_], A](a: Free[F, A])(implicit F: Monad[F]): F[A] = step(a) match {
    case Return(a) => F.unit(a)
    case Suspend(fa) => fa
    case FlatMap(x, f) => x match {
      case Suspend(fa) => F.flatMap(fa) { a => run(f(a)) }
      case _ => sys.error("")
    }

  }
  @annotation.tailrec
  def step[F[_], A](a: Free[F, A]): Free[F, A] = a match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => a
  }
}
