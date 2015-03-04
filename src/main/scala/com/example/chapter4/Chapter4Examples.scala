package com.example.chapter4

import java.util.regex.{Pattern, PatternSyntaxException}

object Chapter4Examples {
  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mkMatcher(pat: String): Option[String => Boolean] =
    pattern(pat) map (p => (s: String) => p.matcher(s).matches)

  def mkMatcher1(pat: String): Option[String => Boolean] =
    for {
      p <- pattern(pat)
    } yield (s: String) => p.matcher(s).matches()


  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      x <- a
      y <- b
    } yield f(x, y)
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap {
      hh => {
        sequence(t).map((list: List[A]) => hh :: list)
      }
    }
  }

  def parsePatterns(a: List[String]): Option[List[Pattern]] =
    sequence(a map pattern)


  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => f(h).flatMap(hh => traverse(t)(f).map((list: List[B]) => hh :: list))
    }
  }

  def test(a: List[String]) = {
    a match {
      case Nil =>
      case x :: y =>
    }
  }


  def main(args: Array[String]) {
    println(map2(Some("hello"), Some(1))((s: String, i: Int) => s + " " + i + " " + s))
    println(map2(Some("hello"), None)((s: String, i: Int) => s + " " + i + " " + s))

    println(sequence(List(Some("hello"), Some("hello"))))
    println(sequence(List(Some("hello"), Some("world"), None)))

    val l1 = List("hello", "hello")
    val g1: Option[List[Pattern]] = parsePatterns(l1)
    println("-----------------")
    val x = List("dd", "ff").map((s: String) => pattern("a"))

    println("-------")
    println(traverse(List("hello", "\\"))(pattern))
  }
}
