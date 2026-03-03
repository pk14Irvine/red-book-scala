package ch3
import List.*

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:

  def sum(ints: List[Int]): Int = ints match
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double = doubles match
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)

  def tail[A](list: List[A]): List[A] = list match
    case Nil => Nil
    case Cons(_, tail) => tail

  def setHead[A](list: List[A], value: A): List[A] = list match
    case Nil => Cons(value, Nil)
    case Cons(_, tail) => Cons(value, tail)

  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n == 0) return list
    list match
      case Nil => Nil
      case Cons(_, tail) => drop(tail, n - 1)
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = list match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (f(head)) dropWhile(tail, f)
      else list
  }

  def init[A](as: List[A]): List[A] = as match
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(hd, tl) => Cons(hd, init(tl))
    
  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def length[A](as: List[A]): Int = 

val exercise1 = List(1, 2, 3, 4, 5) match
  case Cons(x, Cons(2, Cons(4, _))) => x
  case Nil => 42
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  case _ => 101


