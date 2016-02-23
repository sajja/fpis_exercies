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

  def product1(ds: List[Double]): Double = {
    foldRight(ds, 1d)((i, j) => i * j)
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

  def setHead[T](l: List[T], t: T) = l match {
    case Nil => Cons(t, Nil)
    case Cons(_, xs) => Cons(t, xs)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def foldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    def loop(list: List[A], b: B): B = {
      list match {
        case Nil => b
        case Cons(x, xs) => loop(xs, f(x, b))
      }
    }
    loop(l, z)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((a: A, i: Int) => i + 1)

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, List[A]())((a: A, list: List[A]) => Cons(a, list))
  }
  //you lazy ass finish up the rest of the chapter :D
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
  println(List.setHead(List(1, 2, 3, 4, 5), 100))
  println("fold right " + List.foldRight(List(1, 2, 3, 4, 5), 0)((i: Int, j: Int) => i + j))
  println(List.product(List(1, 2, 3)))
  println(List.product1(List(1, 2, 3)))
  println(List.length(List(1, 2, 3, 4, 5, 6, 7, 8, 9)))
  println("Fold left " + List.foldLeft(List(1, 2, 3, 4, 5), 0)((i: Int, j: Int) => i + j))
  println("reverse 1,2,3,4 " + List.reverse(List(1, 2, 3, 4)))
}