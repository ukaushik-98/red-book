package chapter4

import chapter4.Option.unit

enum Option[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(a) => Some(f(a))
      case None    => None
    }

  def mapF[B](f: A => B): Option[B] = flatMap(a => unit(f(a)))

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

  def orElseF[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).orElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) => if f(a) then Some(a) else None
      case None    => None
    }

  def filterF(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then unit(a) else None)

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then Option.None else Option.Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

object Option:
  def unit[A](a: A): Option[A] = Some(a)
