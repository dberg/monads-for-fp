package monadsforfp

import scala.language.higherKinds

object Arrays {

  case class ArrImpl[K, V](default: V, m: Map[K, V]) {
    def get(k: K): V = m.get(k).getOrElse(default)
    def put(k: K, v: V): ArrImpl[K, V] = ArrImpl[K, V](default, m + (k -> v))
  }

  type Id = String
  type Ix = Id // array indexes
  type Val = Int // array values
  type State = Arr
  type Arr = ArrImpl[Id, Val]

  def newarray(v: Val): Arr = ArrImpl[Id, Val](v, Map[Id, Val]())
  def index(ix: Ix, arr: Arr): Val = arr.get(ix)
  def update(ix: Ix, v: Val, arr: Arr): Arr = arr.put(ix, v)

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

  // interpreter
  def eval(t: Term, s: State): Int = t match {
    case Var(i) => index(i, s)
    case Con(a) => a
    case Add(t, u) => eval(t, s) + eval(u, s)
  }

  def exec(c: Comm, s: State): State = c match {
    case Asgn(i, t) => update(i, eval(t, s), s)
    case Seq(c, d) => exec(d, exec(c, s))
    case If(t, c, d) => if (eval(t, s) == 0) exec(c, s) else exec(d, s)
  }

  def elab(p: Prog): Int = eval(p.t, exec(p.c, newarray(0)))

  // Array transformers
  case class StateM[S, A](runS: S => (A, S))
  type StateArr[A] = StateM[Arr, A]

  trait MonadArr[F[_]] extends Monad[F] {
    def block[A](v: Val, fa: F[A]): A
    def fetch(ix: Ix): F[Val]
    def assign(ix: Ix, v: Val): F[Unit]
  }

  implicit val arrMonad = new MonadArr[StateArr] {
    def unit[A](a: A): StateArr[A] = StateM(s => (a, s))

    def flatMap[A, B](fa: StateArr[A])(f: A => StateArr[B]): StateArr[B] = StateM({s =>
      val (a, s2) = fa.runS(s)
      f(a).runS(s2)
    })

    def block[A](v: Val, fa: StateArr[A]): A = {
      val (a, _) = fa.runS(newarray(v))
      a
    }

    def fetch(ix: Ix): StateArr[Val] =
      StateM[Arr, Val](x => (index(ix, x), x))

    def assign(ix: Ix, v: Val): StateArr[Unit] =
      StateM[Arr, Unit](x => ((), update(ix, v, x)))
  }

  def run(): Unit = {
    // Sample program 1
    println(runProgram1())
    println(runProgram2())
    println(runProgram3())
  }

  def runProgram1(): Int = {
    val p = Prog(Asgn("foo", Con(1)), Var("foo"))
    elab(p)
  }

  def runProgram2(): Int = {
    val t = Add(Var("foo"), Var("bar"))
    val c = Seq(Asgn("foo", Con(1)), Asgn("bar", Con(2)))
    val p = Prog(c, t)
    elab(p)
  }

  def runProgram3(): Int = {
    val c = If(Var("foo"), Asgn("foo", Con(100)), Asgn("foo", Con(200)))
    val p = Prog(c, Var("foo"))
    elab(p)
  }

}
