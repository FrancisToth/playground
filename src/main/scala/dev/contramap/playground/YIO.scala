package dev.contramap.playground

import scala.annotation.tailrec

sealed trait YIO[+A] { self =>
  def map[B](f: A => B): YIO[B] =
    YIO.FlatMap(self)(a => YIO.Return(f(a)))

  def flatMap[A0 >: A, B](
      f: A => YIO[B]
  ): YIO[B] =
    YIO.FlatMap(self)(f)

  def tap(f: A => Unit): YIO[A] =
    map { a => f(a); a }
}

object YIO {
  case class Return[+A](value: A) extends YIO[A]

  final class FlatMap[A, +B](val yio: YIO[A])(val f: A => YIO[B]) extends YIO[B]
  object FlatMap {
    def apply[A, B](yio: YIO[A])(f: A => YIO[B]): YIO[B] =
      new FlatMap(yio)(f)

    def unapply[A, B](fl: FlatMap[A, B]): Option[(YIO[A], A => YIO[B])] =
      Some((fl.yio, fl.f))
  }

  // Not tail recursive, may throw a StackOverflow exception
  def unsafeRun[A]: YIO[A] => A = {
    case Return(value)      => value
    case fl: FlatMap[a0, A] => unsafeRun(fl.f(unsafeRun(fl.yio)))
  }

  // Optimized and tail-recursive run (no stackoverflow)
  @tailrec
  def run[A](yio: YIO[A]): A =
    optimize(yio) match {
      case Return(value) => value
      case fl: FlatMap[a0, A] =>
        fl.yio match {
          case Return(value) => run(fl.f(value))
          case _ => throw new AssertionError("Unreachable")
        }
    }

  // Optimizes the program's representation by leveraging
  // the Associativity law and fusing nested FlatMap by composing their
  // mapping function
  @tailrec
  private def optimize[A](yio: YIO[A]): YIO[A] =
    yio match {
      case fl: FlatMap[a0, A] =>
        fl.yio match {
          case Return(value) => optimize(fl.f(value))
          case fl0: FlatMap[a1, a0] => 
            optimize(FlatMap(fl0.yio)(fl0.f.andThen(_.flatMap(fl.f))))
        }
      case _ => yio
    }
}
/*For those who attended yesterday's session about JDG's article, I went a bit further and had some fun with a tail-recursive interpreter for a data-structure representing a program in general:*/
// object Example extends App {
//   def loop: YIO[Unit] = YIO.Return(42).tap(println).flatMap(_ => loop)

//   YIO.run(loop)
// }

sealed trait YIO2[-R, +A] { self =>
  def map[B](f: A => B): YIO2[R, B] =
    YIO2.FlatMap(self, (a: A) => YIO2.Return(f(a)))

  def flatMap[R0 <: R, A0 >: A, B](
      f: A => YIO2[R0, B]
  ): YIO2[R0, B] =
    YIO2.FlatMap(self, f)

  def tap(f: A => Unit): YIO2[R, A] =
    map { a => f(a); a }

  // def zip[R0 <: R, B](that: YIO2[R0, B]): YIO2[R0, (A, B)] =
  //   YIO2.Zip(self, that)
}

object YIO2 {
  case class Return[+A](value: A) extends YIO2[Any, A]

  case class FlatMap[R, A, +B](yio: YIO2[R, A], f: A => YIO2[R, B])
      extends YIO2[R, B]

  // case class FlatMap2[R, R0 <: R, A, +B](yio: YIO2[R0, A], f: A => YIO2[R0, B])
  //     extends YIO2[R, B]

  // case class Read[R, A](f: R => YIO2[R, A]) extends YIO2[R, A]

  // case class Provide[R, A](env: R, yio: YIO2[R, A]) extends YIO2[Any, A]

  // case class Zip[R, A, B](left: YIO2[R, A], right: YIO2[R, B])
  //     extends YIO2[R, (A, B)]

  // case class Provide2[R, R0 <: R, A](env: R, yio: YIO2[R0, A]) extends YIO2[Any, A]

  // trait Union[R0, R1] extends R0 with R1
  // def union[R0, R1](env0: R0, env1: R1): R0 with R1 = ???

  // Not tail recursive, may throw a StackOverflow exception
  def unsafeRun[R, A]: YIO2[R, A] => R => A = {
    case Return(value)        => _ => value
    case fl: FlatMap[R, a, A] => r => unsafeRun(fl.f(unsafeRun(fl.yio)(r)))(r)
    // case yio: Read[R, A]      => r => unsafeRun(yio.f(r))(r)
    // case pr: Provide[r, A]    => _ => unsafeRun(pr.yio)(pr.env)
    // case zip: Zip[R, a0, b0] =>
    //   r => (unsafeRun(zip.left)(r), unsafeRun(zip.right)(r))
  }

  // Optimized and tail-recursive run (no stackoverflow)
  @tailrec
  def run[R, A](yio: YIO2[R, A])(env: R): A =
    optimize2(env, yio) match {
      case Return(value) => println("test1"); value
      // case yio: Read[R, A]    => run(yio.f(env))(env)
      // case pr: Provide[r0, A] => run(pr.yio)(pr.env)
      // case z: Zip[R, a0, b0] =>
      //   run(for {
      //     a0 <- z.left
      //     b0 <- z.right
      //   } yield (a0, b0): A)(env)

      case FlatMap(Return(value), f) => println("test2"); run(f(value))(env)
      // case FlatMap(Read(f0), f1)     => run(f1.compose(f0)(env))(env)
      // case FlatMap(pr: Provide[r0, a0], f) =>
      //   val xx = run(pr.yio.flatMap(f)) _
      //   val x = f(xx)
      //   //val x = y0.flatMap(f)
      //   ???
      case x => throw new AssertionError("Unreachable: " + x)
    }

  // type Aux[R, A, B] = FlatMap[R, A, B] {
  //   val yio: YIO2[Any, ]
  // }
  // Optimizes the program's representation by leveraging
  // the Associativity law and fusing nested FlatMap by composing their
  // mapping function
  @tailrec
  private def optimize2[R, A](env: R, yio: YIO2[R, A]): YIO2[R, A] =
    yio match {
      case FlatMap(Return(value), f) =>
        println("test 3 " + value)
        println("test 33 " + f(value))
        f(value)

      case FlatMap(FlatMap(yio0, f0), f1) =>
        println("test 4 " + yio)
        println("test 5 " + FlatMap(yio0, f0(_: Any).flatMap(f1)))
        optimize2(env, FlatMap(yio0, f0(_: Any).flatMap(f1)))

      // optimize2(env, FlatMap(yio0, f1.compose(f0)))

      // case fl0: FlatMap[R, a0, A] =>
      //   fl0.yio match {
      //     case fl1: FlatMap[R, a1, a0] =>
      //       val y: a1 => YIO2[R, a0] = fl1.f
      //       val x: a0 => YIO2[R, A] = fl0.f

      //       val xxx = FlatMap(fl1.yio, (a1: a1) => {
      //         val z = y(a1)
      //         FlatMap(z, (a0: a0) => x(a0))
      //       })
      //       optimize2(env, fl1.yio.flatMap { a1 => fl1.f(a1).flatMap(fl0.f) })
      //   }

      // case FlatMap(Read(f0), f1) => optimize2(env, FlatMap(f0(env), f1))
      // case FlatMap(Zip(left, right), f) =>
      //   optimize2(env, left.flatMap(a0 => right.flatMap(b0 => f((a0, b0)))))
      case _ =>
        println("test 5")
        yio
    }
}

object Example2 extends App {
  def loop(n: Int): YIO2[Any, Int] =
    YIO2.Return(n).flatMap { n =>
      if (n == 0) YIO2.Return(n) else loop(n - 1)
    }

  //YIO2.run(())(loop(Int.MaxValue))

  val constant: YIO2[Any, Int] =
    YIO2.Return(42).flatMap(_ => constant)

  // val loop2: YIO2[Int, Int] =
  //   YIO2
  //     .Read { (n: Int) => YIO2.Return(n - 1) }
  //     .flatMap { n => if (n == 0) YIO2.Return(n) else loop2 }

  YIO2.run(constant.tap(x => println("TAP " + x)))(10)
}
