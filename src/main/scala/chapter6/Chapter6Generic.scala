package chapter6

//type State[S, +A] = S => (A, S)

case class State[S, +A](run: S => (A, S))

object State:
  def unit[S, A](a: A): State[S, A] =
    State { s =>
      (a, s)
    }

extension [S, A](underlying: State[S, A])
  def map[B](f: A => B): State[S, B] =
    State[S, B] { s =>
      val (a, s2) = underlying.run(s)
      (f(a), s2)
    }

  def mapF[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](action2: State[S, B])(f: (A, B) => C): State[S, C] =
    State[S, C] { s =>
      val (a, s2) = underlying.run(s)
      val (b, s3) = action2.run(s2)
      (f(a, b), s3)
    }

  def map2F[B, C](action2: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => action2.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s2) = underlying.run(s)
      f(a).run(s2)
    }

  // Expanded code with substitution
  // only does 2 state transitions
//  def map2FExpanded[B, C](action2: State[S, B])(f: (A, B) => C): State[S, C] =
//    State[S, C] { s =>
//      val (a, s2) = underlying.run(s) // this simply passes the state forward into the next computation of f. it DOES NOT run the compulation
//      // a => action2.map(b => f(a, b)
//      a =>
//        State { sInner => // sInner = s2
//          val (b, s3) = action2.run(sInner) // this is s3
//          (f(a, b), s3) // computes f and then wires the newly computed state forward. NOTE: since this is map, it has a type has C and returns the next state
//        }
//    }
