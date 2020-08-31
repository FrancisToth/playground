package dev.contramap

import Stream._
import scala.annotation.tailrec

sealed trait Stream[+A] { self =>

  // final def flatMap[B](f: A => Stream[B]): Stream[B] =
  //   FlatMap(self, f)

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    self match {
      case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
      case x             => FlatMap(x, f)
    }

  final def map[B](f: A => B): Stream[B] =
    flatMap(a => Done(f(a)))

  final def run: A =
    resume match {
      case Left(k)  => k().run
      case Right(v) => v
    }

  final def resume: Either[() => Stream[A], A] =
    self match {
      case Done(v)             => Right(v)
      case More(k)             => Left(k)
      case FlatMap(Done(v), f) => f(v).resume
      case FlatMap(More(k), f) => Left(() => FlatMap(k(), f))
      case FlatMap(FlatMap(b, g), f) =>
        FlatMap(b, (x: Any) => FlatMap(g(x), f)).resume
    }

  final def tap(f: A => Unit): Stream[A] =
    map { a => f(a); a }
}

object Stream {
  case class More[+A](k: () => Stream[A]) extends Stream[A]
  case class Done[+A](result: A) extends Stream[A]
  case class FlatMap[A, +B](stream: Stream[A], f: A => Stream[B])
      extends Stream[B]

  @tailrec
  def run2[A0](s: Stream[A0]): A0 =
    resume2(s) match {
      case Left(k)  => run2(k())
      case Right(v) => v
    }

  @tailrec
  def resume2[A0](s: Stream[A0]): Either[() => Stream[A0], A0] =
    s match {
      case Done(v)             => Right(v)
      case More(k)             => Left(k)
      case FlatMap(Done(v), f) => resume2(f(v))
      case FlatMap(More(k), f) => Left(() => k().flatMap(f))
      case FlatMap(FlatMap(b, g), f) => resume2(b.flatMap(x  => g(x).flatMap(f)))
    }
}

object StreamExample extends App {

  /*
    Flatmap(
      Done(100000),
      n0 => FlatMap(
        Done(99999),
        n0 => FlatMap(
          Done(99998),
          ...
        )
      )
    )

    Flatmap(Flatmap(Done(100000), tap), n0 => ...)
                    |----^-----|  |^|   |---^---|
                         b         g         f
    => FlatMap(Done(10000), (x: Any) => tap(x).flatMap(f)).resume

    case FlatMap(Done(v), f) => f(v).resume
    => FlatMap(Done(10000), f).resume

    case FlatMap(Done(v), f) => f(v).resume
    => FlatMap(FlatMap(Done(99999), tap), f).resume

    Flatmap(Flatmap(Done(100000), tap), n0 => ...)

    case FlatMap(FlatMap(b, g), f) =>
        FlatMap(b, (x: Any) => FlatMap(g(x), f)).resume
   */

  def loop(n: Int): Stream[Int] =
    Done(n).tap(println).flatMap(n0 => if (n0 == 0) Done(n0) else loop(n - 1))

  val stream = loop(100000)
  //println(stream)
  // /stream.run
  //run2(stream)
  // stream.test()
  // stream.run
}
