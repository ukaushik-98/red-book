package chapter6

import scala.annotation.tailrec

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

val nonNegativeInt: Rand[Int] = rng =>
  val (a, nextRng) = rng.nextInt
  (if a < 0 then -(a + 1) else a, nextRng)

val double: Rand[Double] = rng =>
  val (a, nextRng) = rng.nextInt
  (a / (Int.MaxValue.toDouble + 1), nextRng)

val intDouble: Rand[(Int, Double)] = rng =>
  val (a, nextRng) = rng.nextInt
  val (b, nextRng2) = nextRng.nextInt
  ((a, b), nextRng2)

val doubleInt: Rand[(Double, Int)] = rng =>
  val ((a, b), nextRng) = intDouble(rng)
  ((b, a), nextRng)

val double3: Rand[(Double, Double, Double)] = rng =>
  val (a, nextRng) = rng.nextInt
  val (b, nextRng2) = nextRng.nextInt
  val (c, nextRng3) = nextRng2.nextInt
  ((a, b, c), nextRng3)

def ints(count: Int)(rng: RNG): (List[Int], RNG) =
  @tailrec
  def run(rng: RNG)(count: Int)(accum: List[Int]): (List[Int], RNG) =
    if count == 0 then (accum, rng)
    else
      val (a, nextRng) = rng.nextInt
      run(nextRng)(count - 1)(a :: accum)

  run(rng)(count)(List.empty)
