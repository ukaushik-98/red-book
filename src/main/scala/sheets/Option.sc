sealed trait Option[+T] {
  def map[U](f: T => U): Option[U] = this match {
    case Some(t) => Some.of(f(t))
    case None => None
  }

  def flatMap[U](f: T => Option[U]): Option[U] = this match {
    case Some(t) => f(t)
    case None => None
  }
}

case class Some[+T](t: T) extends Option[T]
case object None extends Option[Nothing]


object Some {
  def of[T](t: T): Option[T] = Some(t)
}

val o = Some.of(5)
val o1 = o.map(x => x + 1)
val o2 = o.flatMap(x => Some.of(x))
