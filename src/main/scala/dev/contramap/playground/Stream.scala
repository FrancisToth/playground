package dev.contramap

import scala.annotation.tailrec

final case class Pipe[A, B](process: A => B) {
  def and[C](that: Pipe[B, C]): Pipe[A, C] =
    Pipe(process.andThen(that.process))

  def apply(a: A): B =
    process(a)

  def combine[A0, B0](that: Pipe[A0, B0]): Pipe[(A, A0), (B, B0)] =
    Pipe { case (a, a0) => (process(a), that.process(a0)) }

  def either[C](f: B => Either[B, C]): Pipe[A, Either[B, C]] =
    Pipe(process.andThen(f))

  def zip[C](that: Pipe[A, C]): Pipe[A, (B, C)] =
    zipWith(that)((_, _))

  def zipWith[C, D](that: Pipe[A, C])(f: (B, C) => D): Pipe[A, D] =
    Pipe(a => f(process(a), that.process(a)))
}

sealed trait Chunk0[+A] { self =>

  import Chunk0._

  def concat[A0 >: A](that: Chunk0[A0]): Chunk0[A0] =
    self match {
      case Empty          => that
      case c: Cons[A0, Chunk0[A0]] => Cons(c.head, c.tail.concat(that))
    }

  def either[A0 >: A, B](f: A0 => Either[A0, B]): Chunk0[Either[A0, B]] =
    self match {
      case Empty             => Empty
      case Cons(head, tail) => Cons(f(head), tail.either(f))
    }

  def fold[B](acc: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(acc0: B, rem: Chunk0[A]): B =
      rem match {
        case Empty            => acc
        case Cons(head, tail) => loop(f(head, acc), tail)
      }
    loop(acc, self)
  }

  def zip[B, C](that: Chunk[B]): Chunk[C] = 
    zipWith(that)((_, _))

  def zipWith[B, C](that: Chunk[B])(f: (A, B) => C): Chunk[C] = 
    (self, that) match {
      case (_, Empty) => Empty
      case (Empty, _) => Empty
      case (Cons(a, tail0), Cons(b, tail1)) => Cons(
        f(a, b), 
        tail0.zipWith(tail1)
      )
    }
    
}
object Chunk0 {
  case object Empty extends Chunk0[Nothing]
  case class Cons[+A, T <: Chunk0[A]](head: A, tail: T) extends Chunk0[A]
}


sealed trait Chunk[+A] { self =>

  import Chunk._

  def concat[A0 >: A](that: Chunk[A0]): Chunk[A0] =
    self match {
      case Empty          => that
      case c: Cons[A0, _] => Cons(c.value, () => c.tail().concat(that))
    }

  def either[A0 >: A, B](f: A0 => Either[A0, B]): Chunk[Either[A0, B]] =
    self match {
      case Empty             => Empty
      case Cons(value, tail) => Cons(() => f(value()), () => tail().either(f))
    }

  def fold[B](acc: B)(f: (A, B) => B): B = {
    @tailrec
    def loop(acc0: B, rem: Chunk[A]): B =
      rem match {
        case Empty            => acc
        case Cons(next, tail) => loop(f(next(), acc), tail())
      }
    loop(acc, self)
  }

  def zip[B, C](that: Chunk[B]): Chunk[C] = 
    zipWith(that)((_, _))

  def zipWith[B, C](that: Chunk[B])(f: (A, B) => C): Chunk[C] = 
    (self, that) match {
      case (_, Empty) => Empty
      case (Empty, _) => Empty
      case (Cons(value0, tail0), Cons(value1, tail1)) => Cons(
        () => f(value0(), value1()), 
        () => tail0().zipWith(tail1())
      )
    }
    
}
object Chunk {
  case object Empty extends Chunk[Nothing]
  case class Cons[+A, T <: Chunk[A]](value: () => A, tail: () => T)
      extends Chunk[A]
}

final case class Stream0[R, A](run: R => Chunk[A]) {
  def either[B](f: A => Either[A, B]): Stream0[R, Either[A, B]] = 
    Stream0(seed => run(seed).either(f))

  def fold[B](acc: B)(f: (A, B) => B)(seed: R): B =
    run(seed).fold(acc)(f)

  def concat(that: Stream0[R, A]): Stream0[R, A] =
    Stream0(seed => run(seed).concat(that.run(seed)))
}

final case class Stream[R, A](process: R => (R, Option[A])) {

  def concat(that: Stream[R, A]): Stream[R, A] =
    Stream { seed =>
      val (rem, oa) = process(seed)
      oa.fold(that.process(rem))(_ => (rem, oa))
    }

  // def provide(seed: R): Stream[Any, A] =
  //   Stream { (_:Any) =>
  //     val (rem, oa) = process(seed)

  //   }

  @tailrec
  def fold[B](acc: B)(f: (A, B) => B)(seed: R): B = {
    val (rem, oa) = process(seed)
    oa match {
      case Some(a) => fold(f(a, acc))(f)(rem)
      case None    => acc
    }
  }
}

object Stream {

  def fromVector[A]: Stream[Vector[A], A] =
    Stream { vs => (vs.tail, vs.headOption) }

  def fromOption[A]: Stream[Option[A], A] =
    Stream { oa => (None, oa) }

  val int = fromVector[Int]

}
// // sealed trait Chunk[+A]
// // object Chunk {
// //   case object Empty extends Chunk[Nothing]
// //   case class Cons[+A](head: A, tail) extends Chunk[A]
// // }

// object StreamExample {
//   val ints1 = Stream.fromOption[Int]
//   val ints2 = Stream.fromOption[Int]

//   ints1.concat(ints2)

//   ints1.process(Some(42))
// }

// import Stream._
// import scala.annotation.tailrec

// sealed trait Stream[-R, +A] { self =>

//   final def flatMap[R0 <: R, B](f: A => Stream[R0, B]): Stream[R0, B] =
//     FlatMap(self, f)

//   // def flatMap[R0 <: R, B](f: A => Stream[R0, B]): Stream[R0, B] =
//   //   self match {
//   //     case FlatMap(a, g) => FlatMap(a, (x: Any) => g(x) flatMap f)
//   //     case x             => FlatMap(x, f)
//   //   }

//   final def map[B](f: A => B): Stream[R, B] =
//     flatMap(a => Done(f(a)))

//   final def provide(env: R): Stream[Any, A] =
//     Provide(env, self)

//   final def run(env: R): A =
//     resume(env) match {
//       case Left(k)  => k().run(env)
//       case Right(v) => v
//     }

//   final def resume(env: R): Either[() => Stream[R, A], A] =
//     self match {
//       case Done(v)             => Right(v)
//       case More(k)             => Left(k)
//       case Provide(e0, s0)     => s0.resume(env)
//       case Read(f)             => f(env).resume(env)
//       case FlatMap(Done(v), f) => f(v).resume(env)
//       case FlatMap(Read(g), f) => g(env).flatMap(f).resume(env)
//       case FlatMap(More(k), f) => Left(() => FlatMap(k(), f))
//       case FlatMap(Provide(e0, s0), f) =>
//         Left(() =>
//           s0.resume(e0) match {
//             case Left(k)  => k().flatMap(f)
//             case Right(v) => f(v)
//           }
//         )
//       case FlatMap(FlatMap(b, g), f) =>
//         FlatMap(b, (x: Any) => FlatMap(g(x), f)).resume(env)

//     }

//   final def tap(f: A => Unit): Stream[R, A] =
//     map { a => f(a); a }
// }

// sealed trait Foo {
//   type Union[A, B] <: A with B
// }
// sealed trait  Bar extends Foo {
//   def union[A, B](a: A, b: B): Union[A, B]

//   val a: Int = union(1, "")
// }

// sealed trait SUnion {
//   //type Union[A, B] <: (A with B)
//   type :||:[A, B] <: A
// }

// object Stream extends SUnion {

//   // type Union[A, B] = (A, B)
//   //type :||:[A, B] = Union[A, B]

//   type Union[A, B] <: A with B
//   def union[R0, R1](r0: R0, r1: R1): Union[R0, R1] = ??? //(r0, r1)
//   def right[R0, R1](r: R0 :||: R1): R1 = ??? //r._2
//   //def widen[R0, R1](r0: R0, r1)

//   case class More[-R, +A](k: () => Stream[R, A]) extends Stream[R, A]
//   case class Done[-R, +A](result: A) extends Stream[R, A]
//   case class FlatMap[R, A, +B](stream: Stream[R, A], f: A => Stream[R, B])
//       extends Stream[R, B]

// case class FlatMap2[R, R0, A, +B](stream: Stream[R, A], f: A => Stream[R0, B])
//       extends Stream[Union[R, R0], B]

//   case class Provide[R, A](env: R, stream: Stream[R, A]) extends Stream[Any, A]
//   case class Read[R, +A](f: R => Stream[R, A]) extends Stream[R, A]

//   @tailrec
//   def run2[R0, A0](env: R0)(s: Stream[R0, A0]): A0 =
//     resume2(env)(s) match {
//       case Left(k)  => run2(env)(k())
//       case Right(v) => v
//     }

//   //@tailrec
//   def resume2[R0, A0](
//       env: R0
//   )(s: Stream[R0, A0]): Either[() => Stream[R0, A0], A0] =
//     s match {
//       case Done(v)             => Right(v)
//       case More(k)             => Left(k)
//       case Read(f)             => Left(() => f(env))
//       case Provide(e0, stream) => resume2(e0)(stream)
//       case FlatMap(Done(v), f) => resume2(env)(f(v))
//       case FlatMap(Read(g), f) => Left(() => g(env).flatMap(f))
//       case FlatMap(More(k), f) => Left(() => k().flatMap(f))
//       // case FlatMap2(Provide(e0, s0), f) =>
//       //   resume2(union(e0, right(env)))(FlatMap2(s0, f))

//       case fl: FlatMap2[r1, r2, a1, A0] => fl.stream match {
//         case p: Provide[r3, a1] =>
//           // R0 === r1  with r2
//           // R0 === Any with r2
//           // R0 === Any

//           // r1 :||: r2 === env
//           val x: Stream[Union[r3, r2], A0] = FlatMap2(p.stream, fl.f)
//           // val y = FlatMap2(p.stream, fl.f)
//           //val e = union(p.env, env)

//           /*
//             R0 === r1 :||: r2
//             R0   r1 :||: (r3 :||: r2)
//           */

//           //val e0: Union[r3, R0] = union[r3, R0](p.env, env)

//           //val e2: R0 = e1

//           //val y: R0 = union(r, l)
//           /*
//             Either[() => dev.contramap.Stream[(r3, r2),A0] ,A0]
//             Either[() => dev.contramap.Stream[R0      ,A0] ,A0]
//           */
//           //resume2[R0, A0](e0)(x)

//           // resume2[r3 :||: r2, A0](
//           //   union(p.env, right(env)
//           // ))(FlatMap2(p.stream, fl.f))
//           ???
//       }

//       case FlatMap(Provide(e0, s), f) =>
//         println("test")
//         resume2(e0)(s).fold(
//           g => Left(() => g().flatMap(f)),
//           value => Left(() => f(value))
//         )

//       case FlatMap(FlatMap(b, g), f) =>
//         resume2(env)(b.flatMap(x => g(x).flatMap(f)))
//     }

//   def environment[R]: Stream[R, R] =
//     Read(r => Done(r))
// }

// object StreamExample extends App {

//   /*
//     Flatmap(
//       Done(100000),
//       n0 => FlatMap(
//         Done(99999),
//         n0 => FlatMap(
//           Done(99998),
//           ...
//         )
//       )
//     )

//     Flatmap(Flatmap(Done(100000), tap), n0 => ...)
//                     |----^-----|  |^|   |---^---|
//                          b         g         f
//     => FlatMap(Done(10000), (x: Any) => tap(x).flatMap(f)).resume

//     case FlatMap(Done(v), f) => f(v).resume
//     => FlatMap(Done(10000), f).resume

//     case FlatMap(Done(v), f) => f(v).resume
//     => FlatMap(FlatMap(Done(99999), tap), f).resume

//     Flatmap(Flatmap(Done(100000), tap), n0 => ...)

//     case FlatMap(FlatMap(b, g), f) =>
//         FlatMap(b, (x: Any) => FlatMap(g(x), f)).resume
//    */

//   def loop1: Stream[Any, Any] = FlatMap(Provide((), Done(())), (_: Any) => loop1)
//   def loop: Stream[Int, Any] = loop.provide(42).flatMap(_ => loop)

//   run2(42)(loop1)

//   // val loop2: Stream[Any, Int] =
//   //   Done(n).tap(println).flatMap(n0 => if (n0 == 0) Done(n0) else loop(n - 1))

//   //val stream = loop(100000)
//   //println(stream)
//   // /stream.run
//   //run2(100000)(loop2.flatMap(x => Provide(x, Done(x))))
//   // stream.test()
//   // stream.run
// }
