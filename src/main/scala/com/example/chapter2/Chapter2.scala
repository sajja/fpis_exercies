package com.example.chapter2

import scala.language.postfixOps

object Chapter2 {
  def abs(i: Int) = println("abs function")

  def factorial(n: Int): Int = {
    def loop(i: Int, acc: Int): Int = {
      if (i == 0) acc
      else loop(i - 1, i * acc)
    }
    loop(n, 1)
  }

  def fib(i: Int): Int = {
    def loop(i: Int, acc: Int): Int = {
      if (i == 0) 0
      else if (i == 1) 1
      else loop(i - 1, acc) + loop(i - 2, acc)
    }
    loop(i, 0)
  }

  def format(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def functionObject() = new Function2[Int, Int, Int] {
    override def apply(v1: Int, v2: Int): Int = v1 + v2
  }

  def isSorted[T](arr: Array[T], f: (T, T) => Boolean): Boolean = {
    if (arr.tail isEmpty) true
    else if (f(arr.head, arr.tail.head)) isSorted(arr.tail, f)
    else false
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = ((p: A) => (q: B) => f(p, q)) (a)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (x: A) => f(g(x))
}

object Chapter2Test extends App {

  import Chapter2._

  println(factorial(2))
  println(fib(5))

  println(format("factorial", 2, factorial))
  println(format("fib", 4, fib))
  println(functionObject()(1, 2))

  var strArr = Array("a", "b", "c", "d")
  println(s"String array is sorted = ${isSorted(strArr, (a: String, b: String) => if (a.compareTo(b) <= 0) true else false)}")

  strArr = Array("ax", "ab", "c", "d", "f")
  println(s"String array is sorted = ${isSorted(strArr, (a: String, b: String) => if (a.compareTo(b) <= 0) true else false)}")

  var intArr = Array(1, 2, 3, 4, 5, 0)
  println(s"Int array is sorted = ${isSorted(intArr, (a: Int, b: Int) => a <= b)}")
}
