package dev.contramap.playground

import scala.annotation.tailrec

sealed trait TIO[-R, +A] { self =>
  def flatMap[R0 <: R, B](f: A => TIO[R0, B]): TIO[R0, B] =
    TIO.FlatMap(self, f)

  def map[B](f: A => B): TIO[R, B] =
    flatMap(a => TIO.Done(f(a)))

  def tap(f: A => Unit): TIO[R, A] =
    map { a => f(a); a }

  def zip[R0 <: R, B](tio0: TIO[R0, B]): TIO[R0, (A, B)] =
    TIO.Zip(self, tio0, (a: A, b: B) => (a, b))
  //flatMap(a => tio0.map(b => (a, b)))
}

object TIO {
  case class Done[+A](value: A) extends TIO[Any, A]
  case class More[R, A](f: () => TIO[R, A]) extends TIO[R, A]
  case class FlatMap[R, A, B](tio: TIO[R, A], f: A => TIO[R, B])
      extends TIO[R, B]
  case class Zip[-R, A, B, +C](
      left: TIO[R, A],
      right: TIO[R, B],
      f: (A, B) => C
  ) extends TIO[R, C]

  @tailrec
  def run[R, A](env: R)(tio: TIO[R, A]): A =
    resume(env)(tio) match {
      case Left(k)  => run(env)(k())
      case Right(v) => v
    }

  @tailrec
  def resume[R, A](env: R)(tio: TIO[R, A]): Either[() => TIO[R, A], A] =
    tio match {
      case Done(value) => Right(value)
      case More(f)     => Left(f)
      case Zip(left, right, f) => 
        resume(env)(left.flatMap(a => right.map(b => f(a, b))))
      case FlatMap(FlatMap(t0, f0), f1) =>
        resume(env)(FlatMap(t0, f0.andThen(_.flatMap(f1))))
      case FlatMap(Done(value), f) =>
        resume(env)(f(value))
      case FlatMap(More(f0), f1) =>
        resume(env)(More(() => f0().flatMap(f1)))
      case FlatMap(Zip(l, r, f0), f1) =>
        resume(env)(l.flatMap(l0 => r.flatMap(r0 => f1(f0(l0, r0)))))
    }
}

object TIOExample extends App {
  import TIO._
  // val loop: TIO[Any, Nothing] = Done(42).tap(println).flatMap(_ => loop)
  // run(())(loop)

  val s0: TIO[Any, Int] = Done(0).flatMap(_ => s0)
  val s1: TIO[Any, String] = Done("a").flatMap(_ => s1)

  // FlatMap(FlatMap(Done(0), _ => FlatMap(Done0, _ => ...)), _ => println)
  val program = s0.tap(println) //.zip(s1)
  //println(program)
  run(())(program)

}
