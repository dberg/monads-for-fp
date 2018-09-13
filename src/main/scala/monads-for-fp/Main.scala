package monadsforfp

import scala.language.higherKinds

/** Based on Monads for functional programming:
  * http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
  */
trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

sealed trait Term
case class Con(c: Int) extends Term
case class Div(l: Term, r: Term) extends Term

object Evaluator {
  val answer = Div(Div(Con(1972), Con(2)), Con(23))
  val error = Div(Con(1), Con(0))
}

/** The identity monad:
  * `Id` is the identity function on types,
  * `unit` is the identity function and
  * `flatMap` is application.
  */
object VariationZeroBasicEval {

  // type M a = a
  type Id[A] = A

  implicit val idMonad = new Monad[Id] {
    def unit[A](a: A): Id[A] = a
    def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
  }

  def eval[F[_]](term: Term)(implicit M: Monad[F]): F[Int] = term match {
    case Con(c) => M.unit(c)
    case Div(t, u) => M.flatMap(eval(t)(M)) { a =>
      M.flatMap(eval(u)(M)) { b =>
        M.unit(a / b)
      }
    }
  }

  def run(): Unit = {
    val result: Id[Int] = eval(Evaluator.answer)
    println(result) // 42
  }

}

/** We raise an `Exception` if we find a division by zero. Otherwise we return a
  * value. */
object VariationOneExceptions {

  // We piggyback on Either.
  // Left and Right take the role of Raise and Return.
  // data M a = Raise Exception | Return a
  // type Exception = String
  type Exceptional[A] = Either[String, A]

  trait MonadException[F[_]] extends Monad[F] {
    def raise[A](ex: String): F[A]
  }

  implicit val exceptionalMonad = new MonadException[Exceptional] {
    def unit[A](a: A): Exceptional[A] = Right(a)
    def flatMap[A, B]
      (fa: Exceptional[A])
      (f: A => Exceptional[B]): Exceptional[B] = fa match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
    }
    def raise[A](ex: String): Exceptional[A] = Left(ex)
  }

  def eval
    (term: Term)
    (implicit M: MonadException[Exceptional]): Exceptional[Int] = term match
  {
    case Con(c) => M.unit(c)
    case Div(t, u) => M.flatMap(eval(t)(M)) { a =>
      M.flatMap(eval(u)(M)) { b =>
        // We replace: `M.unit(a / b)` with
        if (b == 0)
          M.raise("divide by zero")
        else
          M.unit(a / b)
      }
    }
  }

  def run(): Unit = {
    val resultSuccess: Exceptional[Int] = eval(Evaluator.answer)
    println(resultSuccess) // Right(42)
    val resultFailure: Exceptional[Int] = eval(Evaluator.error)
    println(resultFailure) // Left(divide by zero)
  }

}

/** A computation accepts an initial state and returns a value. In our example
  * the state is a count of the number divisions executed. */
object VariationTwoState {

  // type M a = State -> (a, State)
  // type State = Int
  case class State[S, A](runS: S => (A, S))
  type StateInt[A] = State[Int, A]

  trait MonadState[F[_]] extends Monad[F] {
    def tick(): F[Unit]
  }

  implicit val stateMonad = new MonadState[StateInt] {
    def unit[A](a: A): StateInt[A] = State(s => (a, s))
    def flatMap[A, B](fa: StateInt[A])(f: A => StateInt[B]): StateInt[B] = State({ s =>
      val (a, s2) = fa.runS(s)
      f(a).runS(s2)
    })
    def tick(): StateInt[Unit] = State(s => ((), s + 1))
  }

  def eval(term: Term)(implicit M: MonadState[StateInt]): StateInt[Int] = term match {
    case Con(c) => M.unit(c)
    case Div(t, u) => M.flatMap(eval(t)(M)) { a =>
      M.flatMap(eval(u)(M)) { b =>
        M.flatMap(M.tick())(_ => M.unit(a / b))
      }
    }
  }

  def run(): Unit = {
    val result: StateInt[Int] = eval(Evaluator.answer)
    println(result.runS(0)) // (42,2)
  }

}

/** A computation consists of the output generated paired with the value
  * returned. */
object VariationThreeOutput {

  // Pretty close to the Haskell version:
  // type M a = (Output, a)
  // type Output = String
  type Output = String
  type OutputM[A] = (Output, A)

  trait MonadOutput[F[_]] extends Monad[F] {
    def out(out: Output): F[Unit]
  }

  implicit val outputMonad = new MonadOutput[OutputM] {
    def unit[A](a: A) = ("", a)
    def flatMap[A, B]
      (fa: OutputM[A])
      (f: A => OutputM[B]): OutputM[B] =
    {
      val (x, a) = fa
      val (y, b) = f(a)
      (x + y, b)
    }
    def out(out: Output): OutputM[Unit] = (out, ())
  }

  def showterm(term: Term): String = term match {
    case Con(c) => s"Con $c"
    case Div(t, u) => "Div (" + showterm(t) + " " + showterm(u) + ")"
  }

  def line(term: Term, a: Int): Output = {
    "eval (" + showterm(term) + ") <= " + a.toString + "\n"
  }

  def eval
    (term: Term)
    (implicit M: MonadOutput[OutputM]): OutputM[Int] =
  {
    term match {
      case Con(a) =>
        M.flatMap(M.out(line(Con(a), a)))(_ => M.unit(a))
      case Div(t, u) =>
        val (x, a) = eval(t)(M)
        val (y, b) = eval(u)(M)
        val r = a / b
        M.flatMap(M.out(x + y + line(Div(t, u), r)))(_ => M.unit(r))
    }
  }

  def run(): Unit = {
    val result: OutputM[Int] = eval(Evaluator.answer)
    println(result._1)
    // eval (Con 1972) <= 1972
    // eval (Con 2) <= 2
    // eval (Div (Con 1972 Con 2)) <= 986
    // eval (Con 23) <= 23
    // eval (Div (Div (Con 1972 Con 2) Con 23)) <= 42
  }

}

object Main {

  def main(args: Array[String]): Unit = {
    VariationZeroBasicEval.run()
    VariationOneExceptions.run()
    VariationTwoState.run()
    VariationThreeOutput.run()
  }

}
