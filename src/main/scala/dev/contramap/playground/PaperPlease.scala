package dev.contramap.playground
import PapersPlease._

object PapersPlease {
  // We keep this as a long for the sake of simplicity
  type Date = Long
  type PassportId = String
  sealed trait Country
  object Country {
    case object Antegria extends Country
    case object Arstotzka extends Country
    case object Impor extends Country
    case object Kolechia extends Country
    case object Obristan extends Country
    case object Republia extends Country
    case object UnitedFederation extends Country
  }

  sealed trait Document
  object Document {
    final case class Passport(
        firstName: String,
        lastName: String,
        expirationDate: Date,
        dob: Date,
        country: Country,
        id: PassportId
    ) extends Document

    final case class EntryPermit(passportId: PassportId, expirationDate: Date)
        extends Document

    final case class IdCard(firstName: String, lastName: String, dob: Date)
        extends Document

    final case class Asylum(
        firstName: String,
        lastName: String,
        dob: Date,
        fingerPrints: String
    )
    final case class FingerPrints(prints: String)
  }
  import Document._
  import Result._

//   sealed trait Papers extends Product with Serializable {
//     val passport: Passport
//   }
//   object Papers {
//     case class Citizen(passport: Passport, idCard: IdCard) extends Papers
//     case class Visitor(passport: Passport, entryPermit: EntryPermit)
//         extends Papers
//   }

  sealed trait Result { self =>
    def &&(that: Result): Result =
      (self, that) match {
        case (_, Aborted)  => that
        case (Approved, _) => that
        case _             => self
      }

    def ||(that: Result): Result =
      self match {
        case Detained | Denied => that
        case _                 => self
      }
  }
  object Result {
    // Second best practice: primitives that cover the full space of solutions
    case object Approved extends Result
    case object Denied extends Result
    case object Detained extends Result
    case object Aborted extends Result
  }
  case class Context[+A](now: Date, payload: A) { self =>
    def as[B](payload0: B): Context[B] =
      map(_ => payload0)

    def map[B](f: A => B): Context[B] =
      Context(now, f(payload))

    // def zip[A0 <: A](that: Rule2[A0]): Rule2[A0] =
    //   //bothWith(that)(c => (c, c))
    //   zipWith(that)(_ && _)

    // def zipWith[A0 >: A, B](
    //     that: Context[A0]
    // )(f: (A, A0) => B): Context[B] =
    //   Context(now, f(self.payload, that.payload))
  }

  /*case class Pipeline[-A, +B](run: A => B) { self =>
  // Makes sense, we act on the left side of the abstract function and keep
  // its output shape
  def eitherWith[A0, B0 >: B, C](
      that: Pipeline[A0, B0]
  )(f: C => Either[A, A0]): Pipeline[C, B0] =
    Pipeline[C, B0] { ctx =>
      f(ctx) match {
        case Left(a)  => self.run(a)
        case Right(b) => that.run(b)
      }
    }
  // Makes sense, we act on the right side of the abstract function and keep
  // its input shape
  def zipWith[A0 <: A, B0, C](
      that: Pipeline[A0, B0]
  )(f: (B, B0) => C): Pipeline[A0, C] =
    Pipeline[A0, C] { ctx =>
      val r0 = self.run(ctx)
      val r1 = that.run(ctx)
      f(r0, r1)
    }

  def bothWith[A0, B0 >: B, C](
      that: Pipeline[A0, B0]
  )(f: C => (A, A0)): Pipeline[C, B0] =
    Pipeline[C, B0] { ctx =>
      val (a, b) = f(ctx)
      // What should done here considering we do not know how to combine
      // these?
      val left = self.run(a)
      val right = that.run(b)
      ???
    }
}*/

  // Initially: Rule = Context[A] => Result
  case class Rule2[-A](run: Context[A] => Result) { self =>
    def &&[B](that: Rule2[B]): Rule2[(A, B)] =
      both(that)

    def both[B](that: Rule2[B]): Rule2[(A, B)] =
      bothWith(that)(identity)

    def join[A0 <: A](that: Rule2[A0]): Rule2[A0] =
      bothWith(that)(a => (a, a))

    def bothWith[B, C](that: Rule2[B])(f: C => (A, B)): Rule2[C] =
      Rule2[C] { ctx =>
        val (a, b) = ctx.map(f).payload
        self.run(ctx.as(a)) && that.run(ctx.as(b))
      }

    def ||[B](that: Rule2[B]): Rule2[Either[A, B]] =
      either(that)

    def either[B](that: Rule2[B]): Rule2[Either[A, B]] =
      eitherWith(that)(identity)

    def eitherWith[B, C](that: Rule2[B])(f: C => Either[A, B]): Rule2[C] =
      Rule2[C] { ctx =>
        ctx.map(f) match {
          case c @ Context(_, Left(a))  => self.run(c.as(a))
          case c @ Context(_, Right(b)) => that.run(c.as(b))
        }
      }

    // def orElse[A0 <: A](that: Rule2[A0]): Rule2[A0] =
    //  zipWith(that)(_ || _)

    // def andThen[A0 <: A](that: Rule2[A0]): Rule2[A0] =
    //  zipWith(that)(_ && _)

    def orElse[A0 <: A](that: Rule2[A0]): Rule2[A0] =
      zipWith(that)(_ || _)

    def zip[A0 <: A](that: Rule2[A0]): Rule2[A0] =
      //bothWith(that)(c => (c, c))
      zipWith(that)(_ && _)

    def zipWith[A0 <: A](
        that: Rule2[A0]
    )(f: (Result, Result) => Result): Rule2[A0] =
      Rule2[A0] { ctx =>
        val r0 = self.run(ctx)
        val r1 = that.run(ctx)
        f(r0, r1)
      }
  }

//   case class Rule3[-A](run: A => Boolean) { self =>
//   // Makes sense, we act on the left side of the abstracted function and keep
//   // its output shape
//   def eitherWith[A0, C](that: Rule3[A0])(f: C => Either[A, A0]): Rule3[C] =
//     Rule3[C] { ctx =>
//       f(ctx) match {
//         case Left(a)  => self.run(a)
//         case Right(b) => that.run(b)
//       }
//     }
//   // Makes sense, we act on the right side of the abstracted function and keep
//   // its input shape
//   def zipWith[A0 <: A, C](that: Rule3[A0])(f: (Boolean, Boolean) => Boolean): Rule3[A0] =
//     Rule3[A0] { ctx =>
//       val r0 = self.run(ctx)
//       val r1 = that.run(ctx)
//       f(r0, r1)
//     }

//   def bothWith[A0, C](that: Rule3[A0])(f: C => (A, A0)): Rule3[C] =
//     Rule3[C] { ctx =>
//       val (a, b) = f(ctx)
//       val left = self.run(a)
//       val right = that.run(b)
//       left && right // My questioning started here
//     }
// }

  object Rule2 {
    val passportIsNotExpired2: Rule2[Passport] = Rule2[Passport] {
      case Context(now, passport) =>
        if (passport.expirationDate <= now) Approved else Denied
    }

    val entryPermitIsNotExpired2: Rule2[EntryPermit] = Rule2[EntryPermit] {
      case Context(now, permit) =>
        if (permit.expirationDate <= now) Approved else Denied
    }

    val always2: Rule2[Any] = Rule2(_ => Approved)
    val hasIdCard2: Rule2[IdCard] = always2
    val hasPassport: Rule2[Passport] = always2
    val hasEntryPermit: Rule2[EntryPermit] = always2
  }
  import Rule2._
  val foreignerRule = passportIsNotExpired2 && entryPermitIsNotExpired2
  val citizenRule2 = passportIsNotExpired2 && hasIdCard2

  val game0 = foreignerRule || citizenRule2

  val hasValidPassport: Rule2[Passport] =
    hasPassport.join(passportIsNotExpired2)
  val hasValidEntryPermit: Rule2[EntryPermit] =
    hasEntryPermit.join(entryPermitIsNotExpired2)

  passportIsNotExpired2.both(hasIdCard2)

  val hasPapers = hasValidPassport && (hasValidEntryPermit || hasIdCard2)

  val idCardIsOk: Rule2[(Passport, IdCard)] = Rule2 { ctx =>
    val (passport, idCard) = ctx.payload
    if (
      passport.dob == idCard.dob &&
      passport.firstName == idCard.firstName &&
      passport.lastName == idCard.lastName
    ) Approved
    else Detained
  }

  val entryIsOk: Rule2[(Passport, EntryPermit)] = Rule2 { ctx =>
    val (passport, permit) = ctx.payload
    if (
      passport.dob == idCard.dob &&
      passport.firstName == idCard.firstName &&
      passport.lastName == idCard.lastName
    ) Approved
    else Detained
  }

  val xx: Rule2[(Passport, Either[EntryPermit, IdCard])] =
    entryIsOk.eitherWith(idCardIsOk) {
      case (passport, Right(idCard)) => Right((passport, idCard))
      case (passport, Left(permit))  => Left((passport, permit))
    }

  val isRefugee: Rule2[(Asylum, FingerPrints)] = Rule2 { ctx =>
    val (prints, permit) = ctx.payload
    if (prints.fingerPrints == permit.prints) Approved else Detained
  }

  val gameX = hasPapers.join(xx).either(isRefugee)
  //val hhhh = passportIsNotExpired2.zipWith(hasIdCard2)(_ && _)

  //passportIsNotExpired2.both(hasIdCard2).zipWith()
  val pass: Passport = ???
  val idCard: IdCard = ???

//   run(hhhh)(Context(0, pass && idCard))

  def run[A](rule: Rule2[A])(env: Context[A]): Result = {
    rule.run(env)
  }

//   val zzzz = entryPermitIsNotExpired2 || hasIdCard2
//   val cccc = zzzz.andThen(passportIsNotExpired2)
//   val xsss =
//     passportIsNotExpired2.andThen(zzzz)

  sealed trait Rule[-A] { self =>

    def run: Context[A] => Result

    // First best practice: binary operators
    // product composition: zip and zipWith falls into this category

    def map(f: Result => Result): Rule[A] =
      Rule[A](ctx => f(self.run(ctx)))

    def zip[B](that: Rule[B]): Rule[(A, B)] =
      zipWith(that)(_ && _)

    def zipWith[B](that: Rule[B])(f: (Result, Result) => Result): Rule[(A, B)] =
      Rule[(A, B)] { ctx =>
        val r0 = self.run(ctx.map(_._1))
        val r1 = that.run(ctx.map(_._2))
        f(r0, r1)
      }

    def zip2[A0 <: A](that: Rule[A0]): Rule[A0] =
      zipWith2(that)(_ && _)

    def zipWith2[A0 <: A, B](
        that: Rule[A0]
    )(f: (Result, Result) => Result): Rule[A0] =
      Rule[A0] { ctx =>
        val r0 = self.run(ctx)
        val r1 = that.run(ctx)
        f(r0, r1)
      }

    def zipWith3[A0 <: A, B](
        that: Rule[A0]
    )(f: (Result, Result) => Result): Rule[A0] =
      Rule[A0] { ctx =>
        val r0 = self.run(ctx)
        val r1 = that.run(ctx)
        f(r0, r1)
      }

    // sum composition
    def or[A0 <: A](that: Rule[A0]): Rule[A0] =
      Rule(ctx => self.run(ctx) || that.run(ctx))

    def ||[B](that: Rule[B]): Rule[Either[A, B]] =
      either(that)

    def either[B](that: Rule[B]): Rule[Either[A, B]] =
      eitherWith(that)(identity)

    def eitherWith[B, C](that: Rule[B])(f: C => Either[A, B]): Rule[C] =
      Rule[C] { ctx =>
        ctx.map(f) match {
          case c @ Context(_, Left(a))  => self.run(c.as(a))
          case c @ Context(_, Right(b)) => that.run(c.as(b))
        }
      }

    def andThen[A0 <: A](that: Rule[A0]): Rule[A0] =
      Rule[A0](ctx => self.run(ctx) && that.run(ctx))

    // product composition
    // Combines two independents rules
    def &&[B](that: Rule[B]): Rule[(A, B)] =
      both(that)
    //   Rule[(A, B)](ctx =>
    //     self.run(ctx.map(_._1)) &&
    //       that.run(ctx.map(_._2))
    //   )

    def contramap[B](f: B => A): Rule[B] =
      Rule[B](ctx => self.run(ctx.map(f)))

    def both[B](that: Rule[B]): Rule[(A, B)] =
      bothWith(that)(identity)

    def bothWith[B, C](that: Rule[B])(f: C => (A, B)): Rule[C] =
      Rule[C] { ctx =>
        val (a, b) = ctx.map(f).payload
        self.run(ctx.as(a)) && that.run(ctx.as(b))
      }
    //(self && that).contramap(f)

  }
  import Result._
  object Rule {
    def apply[A](f: Context[A] => Result): Rule[A] =
      new Rule[A] { def run: Context[A] => Result = f }

    val passportIsNotExpired: Rule[Passport] = Rule[Passport] {
      case Context(now, passport) =>
        if (passport.expirationDate <= now) Approved else Denied
    }

    val entryPermitIsNotExpired: Rule[EntryPermit] = Rule[EntryPermit] {
      case Context(now, permit) =>
        if (permit.expirationDate <= now) Approved else Denied
    }

    val always: Rule[Any] = Rule(_ => Approved)

    val hasIdCard: Rule[IdCard] = always

    val idCardMatchesPassport = Rule[(Passport, IdCard)] {
      case Context(_, (passport, idCard)) =>
        if (
          passport.dob == idCard.dob &&
          passport.firstName == idCard.firstName &&
          passport.lastName == idCard.lastName
        ) Approved
        else Detained
    }

    // val idCardIsValid: Rule[(Passport, IdCard)] = Rule[(Passport, IdCard)] {
    //   case Context(_, (passport, idCard)) =>
    //     if (
    //       passport.dob == idCard.dob &&
    //       passport.firstName == idCard.firstName &&
    //       passport.lastName == idCard.lastName
    //     ) Approved
    //     else Detained
    // }
  }
  import Rule._
//   // 1 - Define the domain
//   // 2 - try to compose two rules (type Rule) into one and show compose Result
//   // 3 - Add the && operator with variance
//   // 4 - Add the || operator to illustrate sum composition
//   // 5 - Refactor idCardIsValid

//   //val yy = passportIsNotExpired || (passportIsNotExpired && entryPermitIsNotExpired)

  val visitorRule = passportIsNotExpired && entryPermitIsNotExpired

  val citizenRule: Rule[(Passport, IdCard)] =
    passportIsNotExpired
      .both(hasIdCard)
      .andThen(idCardMatchesPassport)

  // Our game could generate the input randomly
  val game = visitorRule || citizenRule

  val x = passportIsNotExpired && (entryPermitIsNotExpired || hasIdCard)

//   val x: Rule[Either[Visitor, Citizen]] =
//     (entryPermitIsNotExpired || idCardIsValid)
//   val y = x && passportIsNotExpired

  //y.run(Context(0L, (Left(), )))

  // def run[A <: Papers](rules: Rule[A])(context: Context[A]): Result = {
  //   rules.map()
  // }
  //passportIsNotExpired.andThen(result => result && entryPermitIsNotExpired)
  // next: add more rules to introduce zip, either, maybe contramap, and the declarative encoding
  // how to make the game?
}