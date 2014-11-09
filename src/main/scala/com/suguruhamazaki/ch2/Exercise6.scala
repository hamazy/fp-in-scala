package com.suguruhamazaki.ch2

object Exercise6 {

  def compose[A, B, C](f: B ⇒ C, g: A ⇒ B): A ⇒ C =
    (a: A) ⇒ f(g(a))
}
