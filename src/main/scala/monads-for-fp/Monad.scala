package monadsforfp

import scala.language.higherKinds

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)(a => unit(f(a)))

  def join[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(fa => fa)

}

// `unit`, `map`, and `join` as primitives.
trait Monad2[F[_]] {

  def unit[A](a: A): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def join[A](ffa: F[F[A]]): F[A]

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    join(map(fa)(a => f(a)))
}
