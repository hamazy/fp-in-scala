package com.suguruhamazaki.ch2

object Exercise3 {

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = f(a, _)
}
