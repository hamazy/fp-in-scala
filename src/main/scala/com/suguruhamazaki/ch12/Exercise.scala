package com.suguruhamazaki.ch12

import scala.language.higherKinds
import scala.language.implicitConversions
import com.suguruhamazaki.ch6.State
import com.suguruhamazaki.ch11.Functor

trait Applicative[F[_]] extends Functor[F] {
  self ⇒

  def unit[A](a: ⇒ A): F[A]

  def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    map2(fa, unit(()))((a, _) ⇒ f(a))

  def traverse[A, B](as: List[A])(f: A ⇒ F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, mbs) ⇒ map2(f(a), mbs)(_ :: _))

  // ex1
  def sequence[A](lma: List[F[A]]): F[List[A]] =
    lma.foldRight(unit(List[A]()))((ma, mla) ⇒ map2(ma, mla)(_ :: _))
  // ex1
  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    if (n <= 0) unit(List())
    else map2(ma, replicateM(n - 1, ma))((a, la) ⇒ a :: la)
  // ex1
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    map2(fa, fb)((_, _))

  // ex2
  // apply in terms of map2 and unit
  def apply[A, B](fab: F[A ⇒ B])(fa: F[A]): F[B] =
    map2(fab, fa)((f, a) ⇒ f(a))
  // map and map2 in terms of apply and unit
  def map_[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    apply(unit(f))(fa)
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) ⇒ C): F[C] =
    apply(apply(unit(f.curried))(fa))(fb)

  // ex3
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) ⇒ D): F[D] =
    apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) ⇒ E): F[E] =
    apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)

  // ex8
  def product[G[_]](G: Applicative[G]) =
    new Applicative[({ type f[x] = (F[x], G[x]) })#f] {
      def unit[A](a: ⇒ A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A ⇒ B], G[A ⇒ B]))(fa: (F[A], G[A])): (F[B], G[B]) =
        (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }

  // ex9
  def compose[G[_]](G: Applicative[G]) =
    new Applicative[({ type f[x] = F[G[x]] })#f] {
      def unit[A](a: ⇒ A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fa: F[G[A]], fb: F[G[B]])(f: (A, B) ⇒ C): F[G[C]] =
        self.map2(fa, fb)((ga, gb) ⇒ G.map2(ga, gb)(f))

      override def apply[A, B](fgab: F[G[A ⇒ B]])(fga: F[G[A]]): F[G[B]] =
        self.apply(self.apply(self.unit { (gab: G[A ⇒ B]) ⇒
          (ga: G[A]) ⇒
            G.apply(G.apply(G.unit { (ab: (A) ⇒ B) ⇒ (a: A) ⇒ ab(a) })(gab))(ga)
        })(fgab))(fga)
    }

  // ex12
  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] =
    ofa.foldRight(unit(Map[K, V]())) { (e: (K, F[V]), z: F[Map[K, V]]) ⇒
      map2(e._2, z) { (v: V, m: Map[K, V]) ⇒
        m + ((e._1, v))
      }
    }
}

object Applicative {
  // ex6
  def validationApplicative[E] = new Applicative[({ type f[x] = Validation[E, x] })#f] {
    def unit[A](a: ⇒ A): Validation[E, A] = Success(a)
    override def apply[A, B](fab: Validation[E, A ⇒ B])(fa: Validation[E, A]): Validation[E, B] =
      fab match {
        case Success(f) ⇒ fa match {
          case Success(a) ⇒ unit(f(a))
          case e: Failure[_] ⇒ e
        }
        case e @ Failure(head, tail) ⇒ fa match {
          case _: Success[_] ⇒ e
          case e2: Failure[_] ⇒ Failure(head, (tail :+ e2.head) ++ e2.tail)
        }
      }
  }
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B] = join(map(fa)(f))

  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa ⇒ fa)

  def compose[A, B, C](f: A ⇒ F[B], g: B ⇒ F[C]): A ⇒ F[C] =
    a ⇒ flatMap(f(a))(g)

  override def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    flatMap(fa)(a ⇒ unit(f(a)))

  override def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) ⇒ C): F[C] =
    flatMap(fa)(a ⇒ map(fb)(b ⇒ f(a, b)))

}

object Monad {
  // ex5
  def eitherMonad[E] = new Monad[({ type f[x] = Either[E, x] })#f] {
    def unit[A](a: ⇒ A): Either[E, A] = Right(a)
    override def flatMap[A, B](fa: Either[E, A])(f: A ⇒ Either[E, B]): Either[E, B] =
      fa.right.flatMap(f)
  }

  type Id[A] = A
  def idMonad = new Monad[Id] {
    def unit[A](a: ⇒ A): A = a
    override def flatMap[A, B](fa: A)(f: A ⇒ B): B = f(fa)
  }

  def stateMonad[S] = new Monad[({ type f[x] = State[S, x] })#f] {
    def unit[A](a: ⇒ A): State[S, A] = State(s ⇒ (a, s))
    override def flatMap[A, B](sa: State[S, A])(f: A ⇒ State[S, B]): State[S, B] =
      sa.flatMap(f)
  }
}

import com.suguruhamazaki.ch10.Foldable
trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_], A, B](fa: F[A])(f: A ⇒ G[B])(implicit G: Applicative[G]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_], A](fga: F[G[A]])(implicit G: Applicative[G]): G[F[A]] =
    traverse(fga)(ga ⇒ ga)

  // ex14
  // in terms of traverse
  def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
    traverse[Monad.Id, A, B](fa)(f)(Monad.idMonad)

  def traverseS[S, A, B](fa: F[A])(f: A ⇒ State[S, B]): State[S, F[B]] =
    traverse[({ type f[x] = State[S, x] })#f, A, B](fa)(f)(Monad.stateMonad)

  import State.{ get, set }
  def zipWithIndex[A](ta: F[A]): F[(A, Int)] =
    traverseS(ta) { (a: A) ⇒
      (for {
        i ← get[Int]
        _ ← set(i + 1)
      } yield (a, i))
    }.run(0)._1

  def toList[A](fa: F[A]): List[A] =
    traverseS(fa) { (a: A) ⇒
      (for {
        as ← get[List[A]]
        _ ← set(a :: as)
      } yield ())
    }.run(Nil)._2.reverse

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) ⇒ (B, S)): (F[B], S) =
    traverseS(fa) { (a: A) ⇒
      (for {
        s1 ← get[S]
        (b, s2) = f(a, s1)
        _ ← set(s2)
      } yield b)
    }.run(s)

  // ex16
  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((a, as) => (as.head, as.tail))._1

  // ex17 in terms of mapAccum
  override def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(as, z) { (a: A, z: B) => ((), f(z, a)) }._2
}

object Traverse {
  // ex13
  def listTraversal = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A ⇒ G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, gfb) ⇒ G.map2(f(a), gfb)(_ :: _))
  }
  def optionTraversal = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A ⇒ G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) ⇒ G.map2(f(a), G.unit(()))((a, _) ⇒ Some(a))
        case None ⇒ G.unit(None)
      }
  }
  def treeTraversal = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A ⇒ G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      G.map2(f(fa.head), listTraversal.traverse(fa.tail)(traverse(_)(f)))(Tree(_, _))
  }
}

sealed trait Validation[+E, +A]
case class Failure[E](head: E, tail: Vector[E] = Vector())
  extends Validation[E, Nothing]
case class Success[A](a: A) extends Validation[Nothing, A]

case class Tree[+A](head: A, tail: List[Tree[A]])
