package ch10

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

val stringMonoid: Monoid[String] = new:
  def combine(a1: String, a2: String) = a1 + a2
  val empty = ""

def listMonoid[A]: Monoid[List[A]] = new:
  def combine(a1: List[A], a2: List[A]) = a1 ++ a2
  val empty = Nil
  
val intAddition: Monoid[Int] = new:
  def combine(a1: Int, a2: Int) = a1 + a2
  val empty = 0

val intMultiplication: Monoid[Int] = new:
  def combine(a1: Int, a2: Int) = a1 * a2
  val empty = 1
  
val booleanOr: Monoid[Boolean] = new:
  def combine(a1: Boolean, a2: Boolean) = a1 || a2
  val empty = false

val booleanAnd: Monoid[Boolean] = new:
  def combine(a1: Boolean, a2: Boolean) = a1 && a2
  val empty = true
  
def optionMonoid[A](fn: (A, A) => A): Monoid[Option[A]] = new:
  def combine(a1: Option[A], a2: Option[A]) = a1.flatMap(aa1 => a2.map(aa2 => fn(aa1, aa2)))
  val empty = Option.empty

def endoMonoid[A]: Monoid[A => A] = new:
  def combine(a1: A => A, b1: A => A) = a1.andThen(b1)
  val empty = identity