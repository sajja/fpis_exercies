package com.example.chapter5

import scala.collection.immutable.Stream.cons

object Chapter5Example {
  def seperator = println("------------------------------------------------------------")

  def square(x: Double): Double = x * x

  def if1[A](cond: Boolean, onTrue: A, onFalse: A): A =
    if (cond) onTrue else onFalse

  def if2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
    if (cond) onTrue else onFalse

  def pair(i: => Int) = (i, i)

  def pair2(i: => Int) = {
    lazy val j = i
    (j, j)
  }

  def toList[T](st: Stream[T]): List[T] = {
    if (st.nonEmpty) {
      st.head :: toList(st.tail)
    } else {
      Nil
    }
  }

  def take[A](st: Stream[A], n: Int): Stream[A] = {
    if (n == 0 || st.isEmpty)
      Stream.empty
    else
      cons(st.head, take(st.tail, n - 1))
  }

  def takeWhile[A](st: Stream[A], p: A => Boolean): Stream[A] = {
    if (st.isEmpty)
      Stream.empty
    else if (p(st.head)) {
      cons(st.head, takeWhile(st.tail, p))
    } else {
      takeWhile(st.tail, p)
    }
  }

  def exists1[A](st: Stream[A], p: A => Boolean): Boolean = {
    if (st.isEmpty) false
    else if (p(st.head)) true
    else exists1(st.tail, p)
  }

  def foldRight[A, B](z: => B)(st: Stream[A], f: (A, => B) => B): B = {
    if (st.isEmpty) {
      z
    } else {
      f(st.head, foldRight(z)(st.tail, f))
    }
  }

  def exists[A](st: Stream[A], p: A => Boolean): Boolean = {
    foldRight[A, Boolean](false)(st, { (x, y) => p(x) || y}: (A, => Boolean) => Boolean)
  }

  def fun1(x: Boolean, y: => Int) = if (x) y else 0

  def xxx[A, B](a: A, b: B, f: (A, => B) => B) = {
    println(f(a, b))
  }

  def forAll[A](st: Stream[A], p: A => Boolean): Boolean = {
    foldRight[A, Boolean](true)(st, { (x, y) => p(x) && y})
  }

  def takeWhile2[A](st: Stream[A], p: A => Boolean): Stream[A] = {
    foldRight[A, Stream[A]](Stream.empty)(st, (a, b) => if (p(a)) cons(a, b) else b)
  }


  def map[T, A](st: Stream[T])(f: T => A): Stream[A] = {
    foldRight[T,Stream[A]](Stream.empty)(st, (a,b)=>cons(f(a),b))
  }

  def filter[T](st: Stream[T])(f: T => Boolean): Stream[T] = {
    takeWhile2(st,f)
  }


  def main(args: Array[String]) {
    square(1)
    try {
      square(sys.error("Fatal Error"))
    } catch {
      case t: RuntimeException => println(t.getMessage)
    }
    seperator
    if1(true, println("This gets printed"), println("this gets printed too"))
    if2(true, println("Only onTrue is printed"), println("this will not get printed"))
    if2(false, println("This never gets printed"), println("only onFalse get printed"))
    seperator
    pair {
      println("hi");
      1 + 41
    }
    seperator
    pair2 {
      println("hi");
      1 + 41
    }
    seperator
    val st = Stream(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    val st1 = Stream(1, 2,3)
    val xx = cons(1, cons(2, Stream.empty))
    val x: List[Int] = toList(xx)
    seperator
    println(toList(take(st, 3)))
    println(toList(takeWhile(st, (i: Int) => i > 10)))
    seperator

    /**
     * This is how to call, anonymous function with by-name parameters.
     */
    val fun: (Int, => Boolean) => Boolean = (x, y) => y
    xxx[Int, Boolean](1, true, fun)
    xxx[Int, Boolean](1, true, { (x, y) => y})
    xxx(1, true, { (x, y) => y}: ((Int, => Boolean) => Boolean))
    //-------------------------------------------------------
    seperator
    println(exists(st, (x: Int) => x == 1))
    seperator
    println(forAll(st, (x: Int) => x < 4))
    println(toList(takeWhile2(st, (x: Int) => (x > 3 && x < 15) || x == 17 || x == 1)))
    seperator
    println(toList(st))
    println(toList(map(st)(_+1)))

    st.map(_*10).filter(_<10)
    println(toList(filter(map(st)(_+10))(_<100)))
  }
}
