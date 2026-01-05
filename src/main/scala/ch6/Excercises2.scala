package ch6

object Excercises2 {

  type Rand[+A] = RNG => (A, RNG)

  trait RNG:
    def nextInt: (Int, RNG)

  case class SimpleRng(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRng = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
  }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](action: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (nxtVal, nxtState) = action(rng)
      (f(nxtVal), nxtState)

  def map2[A, B, C](actionOne: Rand[A])(actionTwo: Rand[B])(f: (A,B) => C): Rand[C] =
    rng =>
      val (aVal, nxtState) = actionOne(rng)
      val (bVal, nxtState2) = actionTwo(nxtState)
      (f(aVal, bVal), nxtState2)

  def both[A, B](actionOne: Rand[A])(actionTwo: Rand[B]): Rand[(A, B)] =
    map2(actionOne)(actionTwo)((a, b) => (a, b))

  val int: Rand[Int] = rng => rng.nextInt

  val nonNegativeInt: Rand[Int] =
    rng =>
      val (num, nxtRng) = rng.nextInt
      if (num < 0) (-(num + 1), nxtRng)
      else (num, nxtRng)

  val nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(num => if (num % 2 == 1) num - 1 else num)

  val double: Rand[Double] =
    rng =>
      val (num, nxtRng) = rng.nextInt
      (num / (Int.MaxValue.toDouble + 1), nxtRng)

  val intDouble: Rand[(Int, Double)] =
    rng =>
      val (numInt, nxtRng) = rng.nextInt
      val (numDouble, nxtRng2) = double(nxtRng)
      ((numInt, numDouble), nxtRng)

  val doubleInt: Rand[(Double, Int)] =
    rng =>
      val ((numInt, numDouble), nxtRng) = intDouble(rng)
      ((numDouble, numInt), nxtRng)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) return (List.empty[Int], rng)
    val (intList, nxtRng) = ints(count - 1)(rng)
    val (numInt, nxtState) = nxtRng.nextInt
    (numInt :: intList, nxtState)
  }
  
  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    def recur(rng: RNG, rs: List[Rand[A]], accum: List[A]): (List[A], RNG) =
      rs match
        case head :: tail =>
          val (a, state) = head(rng)
          recur(state, tail, a :: accum)
        case _ => (accum, rng)
    rng => recur(rng, rs, List.empty[A])
}
