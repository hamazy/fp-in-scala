package com.suguruhamazaki.ch2

object Exercise2 {

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A]): Boolean = 
      as match {
	case Array(a, b, _*) => if (gt(a,b)) go(as.tail) else false
	case Array(a, b) => gt(a, b)
	case _ => true
      }
    go(as)
  }
}
