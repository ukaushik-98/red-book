package chapter6

import scala.annotation.tailrec


object Chapter6 {
  type Rand[+A] = RNG => (A, RNG)

  trait RNG:
    def nextInt: (Int, RNG)

  case class SimpleRng(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
  }

  val int: Rand[Int] = rng => rng.nextInt

  val rng = SimpleRng(42)
  val (r1, rng2) = rng.nextInt
  val (r2, rng3) = rng2.nextInt

  val nonNegativeInt: Rand[Int] = (rng: RNG) =>
    val (n, rng2) = rng.nextInt
    (if n < 0 then -(n + 1) else n, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), rng2)

  val double2: Rand[Double] = map(int)(n => n / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)

  val randIntDouble: Rand[(Int, Double)] = both(int)(double)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)

  val randDoubleInt: Rand[(Double, Int)] = both(double)(int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    @tailrec
    def run(count: Int)(rng: RNG)(accum: List[Int]): (List[Int], RNG) =
      if count == 0 then (accum, rng) else
        val (n, rng2) = rng.nextInt
        run(count - 1)(rng2)(accum ++ List(n))

    run(count)(rng)(List.empty)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

    /*
    * map takes a state action (which is Rand) and a function that does something to the generated state action
    * the equivalent impure code would be:
    * def map(s: RNGObject, f: A => B): B =
    *   val a = s.nextInt
    *   f(a)
    *
    * this is the equivalent of the state transition + function application seen in map as is
    */
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng =>
      val (a, rng2) = s(rng) // state operation to get the current state that we need to apply f to. this is just like map
      f(a)(rng2)

  val nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - (i % 2))

  def map2[A, B, C](ra: Rand[A])(rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def both[A, B](ra: Rand[A])(rb: Rand[B]): Rand[(A, B)] =
    map2(ra)(rb)((_, _)) // same as map2(ra)(rb)((a, b) => (a, b))
}
