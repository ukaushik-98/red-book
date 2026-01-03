trait RNG:
  def nextInt: (Int, RNG)

type Rand[+A] = RNG => (A, RNG)