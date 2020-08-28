package dev.contramap.playground


import scala.annotation.tailrec
import Program._

sealed trait Program[+A] { self =>
  def flatMap[B](f: A => Program[B]): Program[B] =
    Program.FlatMap(self, f)

  def inspect(f: String => String): String =
    self match {
      case Return(value)       => f(s"Return($value)")
      case FlatMap(program, _) => program.inspect(s => f(s"FlatMap($s)"))
      case z: Zip[a, b]        => f(s"Zip(${z.left.inspect(f)}, ${z.right.inspect(f)})")
    }

  def map[B](f: A => B): Program[B] =
    flatMap(a => Program.Return(f(a)))

  def tap(f: A => Unit): Program[A] =
    map { a => f(a); a }

  def zip[B](that: Program[B]): Program[(A, B)] =
    Program.Zip(self, that)
}

object Program {
  case class Return[A](value: A)                                    extends Program[A]
  case class FlatMap[A, B](program: Program[A], f: A => Program[B]) extends Program[B]
  case class Zip[A, B](left: Program[A], right: Program[B])         extends Program[(A, B)]

  def pure[A](a: A): Program[A] =
    Return(a)

  // def run[A](program: Program[A]): A =
  //   optimize(program) match {
  //     case Return(value)             => value
  //     case FlatMap(Return(value), f) => run(f(value))
  //     case Zip(left, right)          => run(left.flatMap(l0 => right.map((l0, _))))
  //     case unreachable               => throw new AssertionError(s"Unreachable: $unreachable")
  //   }

  @tailrec
  def optimize[A](program: Program[A]): Program[A] =
    program match {
      case z: Zip[a, b]                 =>
        optimize(
          for {
            a <- z.left
            b <- z.right
          } yield (a, b): A
        )

      // case FlatMap(Return(value), f)    => optimize(f(value))
      case FlatMap(FlatMap(p0, f0), f1) => optimize(FlatMap(p0, f0.andThen(FlatMap(_, f1))))
      case FlatMap(Zip(left, right), f) =>
        optimize(
          for {
            l <- left
            r <- right
            z <- f((l, r))
          } yield z
        )
      case x                            => x
    }
}

object Example extends App {
  import Program._

  val p1 = pure(42).map(_ + 1).zip(Return("ABC"))
  val p2 = pure(56).map(_ - 1).map(_ - 1).flatMap(_ => p1).zip(Return("DEF"))
  //println(run(p1.zip(p2)))

  def loop(i: Int): Program[Int] = pure(i).tap(println).flatMap(i => if (i == 0) pure(i) else loop(i - 1))

  println(p1.zip(p2).inspect(identity))
  println(optimize(p1.zip(p2)).inspect(identity))

  //run(loop(100000))
}