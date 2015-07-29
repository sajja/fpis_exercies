package com.example.chapter6

import java.util.Date

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) &
        ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int],
        simple(seed2))
    }
  }
}


object Chapter6Example {
  def seperator = println("------------------------------------------------------------")

  def randomPair(rng: RNG): (Int, Int) = {
    val (x, y) = rng.nextInt
    (x, y.nextInt._1)
  }

  def randomPositiveInt(rng: RNG): Int = {
    val rand = rng.nextInt
    if (rand._1 != Int.MinValue) Math.abs(rand._1) else randomPositiveInt(rand._2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (db, next) = rng.nextInt
    if (db <= Int.MaxValue) double(next) else (db.toDouble, next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (int, next) = rng.nextInt
    val dbl = double(next)
    ((int, dbl._1), dbl._2)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    println("ddd")
    count match {
      case 0 => {
        (Nil, rng)
      }
      case _ =>
        val next = rng.nextInt
        (next._1 :: ints(count - 1)(next._2)._1, next._2)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  val int: Rand[Int] = _.nextInt

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }



  def main(args: Array[String]): Unit = {
    println(unit(1)(RNG.simple(1)))
    val rand = RNG.simple(1L)
    println(rand.nextInt._1)
    println(rand.nextInt._1)
    println(rand.nextInt._2.nextInt._1)
    println(rand.nextInt._2.nextInt._1)
    println(rand.nextInt._2.nextInt._2.nextInt._1)
    seperator
    println(randomPair(RNG.simple(100)))
    seperator
    println(randomPositiveInt(rand.nextInt._2))
    seperator
    //    println(double(rand.nextInt._2))
    seperator
    println(ints(4)(rand.nextInt._2)._1)
    seperator
    println(int(rand.nextInt._2)._1)
    seperator
    println(unit(1)(rand)._1)

    println(map[Int, Double](int)((_.toDouble))(rand))
    seperator
  }
}
