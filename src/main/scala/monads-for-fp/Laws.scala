package monadsforfp

import scala.language.higherKinds

object Laws {

  // -----------------------------------------------------------------------------
  // Notes
  // -----------------------------------------------------------------------------
  // A monadic function, kleisli arrows, can be composed
  // def compose[A, B, C](f: A => F[B], g: F[B] => F[C]): A => F[C] =
  //   a => flatMap(f(a))(g)

  // Left and right identities are easier to see
  // compose(f, unit) == f
  // compose(unit, f) == f

  // Left and right identities
  // flatMap(x)(unit) == x
  // flatMap(unit(y))(f) == f(y)

  // Associativity also easier to see with compose
  // compose(compose(f, g), h) == compose(f, compose(g, h))

  // -----------------------------------------------------------------------------
  // Paper
  // -----------------------------------------------------------------------------
  // Left unit
  // Compute the value a, bind b to the result, and compute n.
  // The result is the same as n with value a substituted for variable b.
  // unit a ⋆ λb.n = n[a/b]

  // Right unit
  // Compute m, bind the result to a, and return a.
  // The result is the same as m.
  // m ⋆ λa.unit a = m

  // Associative
  // Compute m, bind the result to a, compute n, bind the result to b, compute o.
  // The order of parentheses in such a computation is irrelevant.
  // m ⋆ (λa.n ⋆ λb.o) = (m ⋆ λa.n) ⋆ λb.o

  // Proving that addition is associative.

  sealed trait Term
  case class Con(i: Int) extends Term
  case class Add(t1: Term, t2: Term) extends Term

  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def unit[A](a: A): Id[A] = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  // eval :: Term -> M Int
  def eval[F[_]](term: Term)(implicit M: Monad[F]): F[Int] = term match {
    // unit a
    case Con(a) => M.unit(a)
    // eval t ⋆ λa.eval u ⋆ λb.unit(a + b)
    case Add(t, u) => M.flatMap(eval(t)(M))(a => M.flatMap(eval(u)(M))(b => M.unit(a + b)))
  }

  // We show that evaluation of
  // Add(t, Add(u, v)) and Add(Add(t, u), v),
  // both compute the same result.

  // Left
  // unit a'⋆λb'.n = n[a'/b']
  // For sake of simplicity we remove `M` references:
  //
  // eval(Add(t, Add(u, v)))
  // flatMap(eval(t))(a => flatMap(eval(Add(u, v)))(x => unit(a + x)))
  // flatMap(eval(t))(a => flatMap(flatMap(eval(u))(b => flatMap(eval(v))(c => unit(b + c))))(x => unit(a + x)))
  // flatmap(eval(t))(a => flatMap(eval(u))(b => flatMap(eval(v))(c => unit(a + (b + c)))))
  //
  // n = unit(a + x)
  // a' = (b + c)
  // b' = x
  // n [a'/b'] = unit(a + (b + c))
  //
  // eval(Add(Add(t, u), v))
  // flatMap(eval(Add(t, u)))(x => flatMap(eval(v))(c => unit(x + c)))
  // flatMap(flatMap(eval(t))(a => flatMap(eval(u))(b => unit(a + b))))(x => flatMap(eval(v))(c => unit(x + c)))
  // flatMap(eval(t))(a => flatMap(eval(u))(b => flatMap(eval(v))(c => unit((a + b) + c))))
  //
  // n = flatMap(eval(v))(c => unit(x + c))
  // a' = (a + b)
  // b' = x
  // n[a'/b'] = flatMap(eval(v))(c => unit((a + b) + c))

  def run(): Unit = {
    // As a simple example:
    val term1 = Add(Con(1), Add(Con(2), Con(3)))
    val term2 = Add(Add(Con(1), Con(2)), Con(3))

    val res1 = eval(term1)
    println(res1) // 6

    val res2 = eval(term2)
    println(res2) // 6
  }

}
