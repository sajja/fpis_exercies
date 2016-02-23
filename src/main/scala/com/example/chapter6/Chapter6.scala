package com.example.chapter6

/**
  * Created by sajith on 2/22/16.
  */
trait RNG {
  def nextInt: (Int, RNG)
}


object RNG {
  type Rand[+A] = RNG => (A, RNG)


  def simple(seed: Long): RNG = new RNG {
    override def nextInt: (Int, RNG) = {
      val seed2 = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }
}

object Chapter6 {

  import RNG._

  val int: Rand[Int] = _.nextInt

  val int1: (RNG => (Int, RNG)) = (j: RNG) => j.nextInt

  def randomPair(rng: RNG): (Int, Int) = {
    val (i, nextRng) = rng.nextInt
    (i, nextRng.nextInt._1)
  }

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, j) = rng.nextInt
    if (i != Int.MinValue) (scala.math.abs(i), j)
    else positiveInt(j)
  }

  //  def positiveInt3: Rand[Int] = map(int)(i => {
  //    if (i != Int.MinValue) scala.math.abs(i)
  //    else
  //  })


  def double(rng: RNG) = rng.nextInt._1.toDouble

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case _ => {
      val nextRng = rng.nextInt
      (nextRng._1 :: ints(count - 1)(nextRng._2)._1, nextRng._2)
    }
  }


  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, j) = rng.nextInt
    val (p, q) = j.nextInt
    ((i, p.toDouble), q)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = {
    r =>
      val (i, j) = rand(r)
      (f(i), j)
  }

  def positiveMaxInt(n: Int): Rand[Int] = map(positiveInt)(i => i % n)

  def double2 = map((rng: RNG) => rng.nextInt)(_.toDouble)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    (a: RNG) => {
      val i = ra(a)
      val j = rb(i._2)
      (f(i._1, j._1), j._2)
    }
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    r: RNG => {
      val (i, rn) = f(r)
      g(i)(rn)
    }
  }

  def mapNew[A, B](rand: Rand[A])(f: A => B): Rand[B] = {
    flatMap(rand)(a => unit(f(a)))
  }

  def map2New[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)((a: A) => map(rb)((b: B) => f(a, b)))

}


object Chapter6Test extends App {

  import Chapter6._

  val rng = RNG.simple(1)
  println("Random pair " + Chapter6.randomPair(RNG.simple(1)))
  println("Abs test1 " + Chapter6.positiveInt(rng))
  println("Abs test2 " + Chapter6.positiveInt(RNG.simple(Int.MinValue)))
  println("Double " + double(rng))
  println("Int list " + ints(10)(rng))
  println("Positive max int " + positiveMaxInt(6)(rng))
  println("Double " + double2(rng))
}
