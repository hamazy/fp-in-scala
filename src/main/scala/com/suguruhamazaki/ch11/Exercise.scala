package com.suguruhamazaki.ch11

import scala.language.higherKinds

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A ⇒ B): F[B]

  def distribute[A, B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A, B](e: Either[F[A], F[B]]): F[Either[A, B]] =
    e match {
      case Left(fa) ⇒ map(fa)(Left(_))
      case Right(fb) ⇒ map(fb)(Right(_))
    }

}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A, B](fa: List[A])(f: A ⇒ B): List[B] =
      fa.map(f)
  }
}

trait Monad[F[_]] extends Functor[F] {

  def unit[A](a: ⇒ A): F[A]

  def flatMap[A, B](ma: F[A])(f: A ⇒ F[B]): F[B]

  def map[A, B](ma: F[A])(f: A ⇒ B): F[B] =
    flatMap(ma)(a ⇒ unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) ⇒ C): F[C] =
    flatMap(ma)(a ⇒ map(mb)(b ⇒ f(a, b)))

  // ex3
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) ⇒ map2(ma, mla)(_ :: _))

  // ex3
  def traverse[A, B](la: List[A])(f: A ⇒ F[B]): F[List[B]] =
    sequence(la.map(f))

  // ex4
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List())
    else map2(ma, replicateM(n - 1, ma))((a, la) ⇒ a :: la)

  // ex6
  def filterM[A](ms: List[A])(f: A ⇒ F[Boolean]): F[List[A]] =
    ms match {
      case Nil ⇒ unit(List())
      case head :: rest ⇒
        val restFiltered = filterM(rest)(f)
        flatMap(f(head)) { b ⇒
          if (b) map(restFiltered) { head :: _ }
          else restFiltered
        }
    }

  // ex7
  def compose[A, B, C](f: A ⇒ F[B], g: B ⇒ F[C]): A ⇒ F[C] = { a ⇒
    flatMap(f(a))(g)
  }

  // ex8
  def flatMap2[A, B](ma: F[A])(f: A ⇒ F[B]): F[B] =
    compose({ _: Unit ⇒ ma }, f)(())

  // ex12
  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(ma ⇒ ma)

  // ex13
  def flatMap3[A, B](ma: F[A])(f: A ⇒ F[B]): F[B] =
    join(map(ma)(f))

  // ex13
  def compose2[A, B, C](f: A ⇒ F[B], g: B ⇒ F[C]): A ⇒ F[C] = { a ⇒
    join(map(f(a))(g))
  }
}

object Monad {

  // ex17
  val idMonad: Monad[Id] = new Monad[Id] {
    def unit[A](a: ⇒ A): Id[A] = Id(a)
    def flatMap[A, B](a: Id[A])(f: A ⇒ Id[B]): Id[B] = f(a.value)
  }

  // ex1
  val optionMonad: Monad[Option] = new Monad[Option] {
    def unit[A](a: ⇒ A): Option[A] = Some(a)
    def flatMap[A, B](a: Option[A])(f: A ⇒ Option[B]): Option[B] =
      a.flatMap(f)
  }

  // ex1
  val listMonad: Monad[List] = new Monad[List] {
    def unit[A](a: ⇒ A): List[A] = List(a)
    def flatMap[A, B](a: List[A])(f: A ⇒ List[B]): List[B] =
      a.flatMap(f)
  }

  import com.suguruhamazaki.ch6.State
  type IntState[A] = State[Int, A]

  val intStateMonad = new Monad[IntState] {
    def unit[A](a: ⇒ A): IntState[A] = State(s ⇒ (a, s))
    def flatMap[A, B](sa: IntState[A])(f: A ⇒ IntState[B]): IntState[B] =
      sa.flatMap(f)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: ⇒ A): State[S, A] = State(s ⇒ (a, s))
    def flatMap[A, B](sa: State[S, A])(f: A ⇒ State[S, B]): State[S, B] =
      sa.flatMap(f)
  }
}

case class Id[A](value: A) {

  // ex17
  def map[B](f: A ⇒ B): Id[B] = Id(f(value))

  // ex17
  def flatMap[B](f: A ⇒ Id[B]): Id[B] = f(value)
}
