package com.example.chapter3

import com.example.chapter2.{Cons, MyList,Nil}

object Chapter3Examples {
  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def head[T](l: MyList[T]): Option[T] = {
    l match {
      case Cons(x, y) => Some(x)
      case Nil => None
    }
  }

  def tail[T](l: MyList[T]): MyList[T] = {
    l match {
      case Cons(x, y) => y
      case Nil => Nil
    }
  }

  def drop[T](l: MyList[T], i: Int): MyList[T] = {
    if (i > 0) {
      drop(tail(l), i - 1)
    } else {
      l
    }
  }


  def printL[T](l: MyList[T]): Unit = {
    l match {
      case Cons(x, xs) => {
        print(x)
        print(" -> ")
        printL(xs)
      }
      case Nil => println("Nil")
    }
  }

  def dropWhile[T](l: MyList[T])(fn: T => Boolean): MyList[T] = {
    l match {
      case Cons(x, y) =>
        if (fn(x)) dropWhile(y)(fn)
        else Cons(x, dropWhile(y)(fn))
      case Nil => l
    }
  }


  def head[T](l: MyList[T], head: T) = {
    l match {
      case Cons(x, y) => Cons(head, y)
      case Nil => Nil
    }
  }


  def append[T](first: MyList[T], second: MyList[T]): MyList[T] = {
    first match {
      case Cons(x, y) => Cons(x, append(y, second))
      case Nil => second
    }
  }

  def append1[T](l1:MyList[T],l2:MyList[T]):MyList[T]={
    foldRight(l1,l2)((x,y)=>Cons(x,y))
  }

  def foldRight[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = {
    l match {
      case Cons(x, y) => f(x, foldRight(y, z)(f))
      case Nil => z
    }
  }

  def foldRightTR[A, B](l: MyList[A], z: B)(f: (A, B) => B): B = {
    def loop(p: MyList[A], acc: B): B = {
      p match {
        case Cons(x, y) => loop(y, f(x, acc))
        case Nil => acc
      }
    }

    loop(l, z)
  }

  def foldLeft[A, B](l: MyList[A], z: B)(f: (B, A) => B): B = {
    def loop(ol: MyList[A], acc: B): B = {
      ol match {
        case Cons(x, y) => loop(y, f(acc, x))
        case Nil => acc
      }
    }
    loop(l, z)
  }

  def reverse2[T](l: MyList[T]): MyList[T] = {
    foldLeft(l, MyList[T]())((x, y) => {
      Cons(y, x)
    })
  }


  def reverse1[T](l: MyList[T]): MyList[T] = {
    def loop(ol: MyList[T], nl: MyList[T]): MyList[T] = {
      ol match {
        case Cons(x, y) => loop(y, Cons(x, nl))
        case Nil => nl
      }
    }
    loop(l, Nil)
  }


  def map[T, A](l: MyList[T])(f: T => A): MyList[A] = {
    l match {
      case Cons(x, y) => Cons(f(x), map(y)(f))
      case Nil => Nil
    }
  }


  //tail call.
  def mapTR[T, A](l: MyList[T])(f: T => A): MyList[A] = {
    def loop(ol: MyList[T], nl: MyList[A]): MyList[A] = {
      ol match {
        case Cons(x, y) => loop(y, Cons(f(x), nl))
        case Nil => nl
      }
    }
    loop(l, MyList[A]())
  }


  def sum1(l: MyList[Int]) =
    foldRight(l, 0.0)(_ + _)

  def sumTR(l: MyList[Int]) =
    foldRight(l, 0.0)(_ + _)

  def len(l: MyList[Int]) = {
    foldRight(l, 0)((x, y) => y + 1)
  }

  def main(args: Array[String]) {
    val mylist1 = MyList[Int](1, 2, 3, 4)
    val mylist2 = MyList(1, "x", 3, 4)
    println(mylist1)
    println(mylist2)

    printL[Int](mylist1)
    printL(Cons(100, mylist1))


    println(head(mylist1))
    println(head(MyList(1)))
    println(head(MyList()))
    println(tail(mylist1))

    println(drop(MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 5))
    val dw = dropWhile(MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)) _
    println(dw(x => x % 2 == 0))

    printL(head(MyList(1, 2, 3, 4, 5, 6), 1000))
    println("Append ....")
    printL(append(MyList(1, 2), MyList(3, 4, 5)))
    printL(append1(MyList(1, 2), MyList(3, 4, 5)))
    println(sum1(mylist1))
    println("Tail rec. sum")
    println(sumTR(mylist1))
    println(len(MyList(1, 2, 3, 4, 5, 6)))

    printL(map(MyList("A", "B", "C", "D"))(_.hashCode))
    val ggg = mapTR(MyList("A", "B", "C", "D"))(_.hashCode)
    printL(mapTR(MyList("A", "B", "C", "D"))(_.hashCode))
    printL(reverse1(MyList(1, 2, 3)))
    printL(reverse2(MyList(1, 2, 3)))

  }
}
