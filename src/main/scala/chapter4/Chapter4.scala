package chapter4

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None    => None
    }
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(a) => f(a)
      case None    => None
    }
  def getOrElse[B >: A](default: => B): Option[B] =
    this match {
      case Some(a) => Some(a)
      case None    => Some(default)
    }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(a) => Some(a)
      case None    => ob
    }
  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) => if f(a) then Some(a) else None
      case None    => None
    }

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then Option.None else Option.Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
//  mean(xs) match {
//    case Option.Some(m) => xs.map(x => math.pow(x - m, 2))
//    case Option.None    => Option.None
//  }
