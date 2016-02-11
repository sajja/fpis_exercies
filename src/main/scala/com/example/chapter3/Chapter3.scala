package com.example.chapter3

trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = if (as.isEmpty) Nil else Cons(as.head, apply(as.tail: _*))

  def tail[T](l: List[T]) = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def drop[T](l: List[T], i: Int): List[T] = {
    (l, i) match {
      case (Nil, _) => Nil
      case (l, 0) => l
      case (Cons(x, xs), i) => drop(xs, i - 1)
    }
  }

  def dropWhile[T](l: List[T], f: T => Boolean): List[T] = {
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case Cons(x, xs) => Cons(x, dropWhile(xs, f))
    }
  }
}

object Chapter3Test extends App {
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h
    case _ => 1010
  }

  println(List.tail(List(1, 2, 3, 4, 5)))
  println(List.drop(List(1, 2, 3, 4, 5), 3))
  println(List.dropWhile(List(1, 2, 3, 4, 5), (x: Int) => x > 3))
}