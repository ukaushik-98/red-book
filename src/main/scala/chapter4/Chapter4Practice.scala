package chapter4

def map2[A, B, C](oa: Option[A])(ob: Option[B])(f: (A, B) => C): Option[C] =
  oa.flatMap(a => ob.map(b => f(a, b)))
