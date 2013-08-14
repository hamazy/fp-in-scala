package com.suguruhamazaki.ch2

object Exercise4 {

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    (a: A) => 
      (b: B) => f(a, b)
}
