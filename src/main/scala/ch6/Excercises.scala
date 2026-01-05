package ch6

object ch6 {
  type Rand[+A] = RNG => (A, RNG)

  trait RNG:
    def nextInt: (Int, RNG)

  def int: Rand[Int] = rng => rng.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  /*
    Map is taking a state action (Rand which is RNG => (A, RNG)) which does some function
    (f: A => B) to create another state action. Instead of explicityly passing in RNG tediously,
    we are having the function accomplish creating/transfering a new state ( val (a, nxtRng) = s(rng) )
   */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, nxtRng) = s(rng)
      (f(a), nxtRng)

  def map2[A, B, C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, nxtRng) = ra(rng)
      val (b, nxtRng2) = rb(nxtRng)
      (f(a, b), nxtRng2)

  def both[A, B](ra: Rand[A])(rb: Rand[B]): Rand[(A, B)] =
    map2(ra)(rb)((a, b) => (a, b))

  def flatmap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, nxtRng) = s(rng) // state operation to get current state
      f(a)(nxtRng)

  case class SimpleRng(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRng = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
  }

  val nonNegativeInt: Rand[Int] =
    rng =>
      val (nxtInt, nxtRng) = rng.nextInt
      (if nxtInt < 0 then -(nxtInt + 1) else nxtInt, nxtRng)

  val double2: Rand[Double] =
    map(int)(nxtInt => nxtInt / (Int.MaxValue.toDouble + 1))

  val randIntDouble: Rand[(Int, Double)] =
    both(int)(double2)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double2)(int)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  def double(rng: RNG): (Double, RNG) =
    val (nxtInt, nxtRng) = rng.nextInt
    (nxtInt / (Int.MaxValue.toDouble + 1), nxtRng)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (nxtInt, nxtRng) = rng.nextInt
    val (nxtDouble, nxtRng2) = double(nxtRng)
    ((nxtInt, nxtDouble), nxtRng2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((nxtInt, nxtDouble), nxtRng) = intDouble(rng)
    ((nxtDouble, nxtInt), nxtRng)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (nxtDouble, nxtRng) = double(rng)
    val (nxtDouble2, nxtRng2) = double(nxtRng)
    val (nxtDouble3, nxtRng3) = double(nxtRng2)
    ((nxtDouble, nxtDouble2, nxtDouble3), nxtRng3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng =>
      rs.foldRight((List.empty, rng))((action, acc) =>
        val (result, rng2) = acc
        val (a1, rng3) = action(rng2)
        (a1 :: result, rng3)
      )

  def sequence2[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldRight(unit(List.empty))((action, acc) =>
      map2(acc)(action)((a, b) => b :: a)
    )

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      val (nxtInt, nxtRng) = rng.nextInt
      (List(nxtInt), nxtRng)
    } else {
      val (nxtList, nxtRng) = ints(count - 1)(rng)
      val (nxtInt, nxtRng2) = nxtRng.nextInt
      (nxtInt :: nxtList, nxtRng2)
    }
  }
}
