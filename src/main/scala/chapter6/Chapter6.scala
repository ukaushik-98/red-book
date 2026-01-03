package chapter6

import scala.annotation.tailrec

object Chapter6 {
  trait RNG:
    def nextInt: (Int, RNG)

  case class SimpleRng(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRng = SimpleRng(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRng)
  }

  val rng = SimpleRng(42)
  val (r1, rng2) = rng.nextInt
  val (r2, rng3) = rng2.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n, rng2) = rng.nextInt
    (if n < 0 then -(n + 1) else n, rng2)

  def double(rng: RNG): (Double, RNG) =
    val (n, rng2) = rng.nextInt
    (n / (Int.MaxValue.toDouble + 1), rng2)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)

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
}
