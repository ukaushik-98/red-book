package chapter4

case class Counter[+A, S](run: S => (A, S))

object Counter:
  def unit[A, S](start: A): Counter[A, S] = Counter { s => (start, s) }

extension [S, A](underlying: Counter[A, S])
  def map[B](f: A => B): Counter[B, S] = Counter { s =>
    val (a, s2) = underlying.run(s)
    (f(a), s2)
  }

  def mapF[B](f: A => B): Counter[B, S] =
    flatMap(a => Counter.unit(f(a)))

  def map2[B, C](action: Counter[B, S])(f: (A, B) => C): Counter[C, S] =
    Counter { s =>
      val (a, s2) = underlying.run(s)
      val (b, s3) = action.run(s2)
      (f(a, b), s3)
    }

  def map2F[B, C](action: Counter[B, S])(f: (A, B) => C): Counter[C, S] =
    flatMap(a => action.map(b => f(a, b)))

  def flatMap[B](f: A => Counter[B, S]): Counter[B, S] = Counter { s =>
    val (a, s2) = underlying.run(s)
    f(a).run(s2)
  }
