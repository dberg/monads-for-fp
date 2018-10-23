package monadsforfp

object Arrays {

  type Id = String

  // Simple imperative language
  // A Term is variable | constant | sum of two terms
  sealed trait Term
  case class Var(id: Id) extends Term
  case class Con(i: Int) extends Term
  case class Add(t1: Term, t2: Term) extends Term

  // A Command is an assignment | sequence of two commands | conditional
  sealed trait Comm
  case class Asgn(id: Id, t: Term) extends Comm
  case class Seq(c1: Comm, c2: Comm) extends Comm
  case class If(t: Term, c1: Comm, c2: Comm) extends Comm

  // A program is command followed by a term
  case class Prog(c: Comm, t: Term)

}
