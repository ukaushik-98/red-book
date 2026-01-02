package chapter4

object Chapter4 {
  case class Counter(run: Int => (Int, Int))

  val inc = Counter(x => (x, x + 1))
  val (curr, next) = inc.run(0)
  inc.run(10)
}
