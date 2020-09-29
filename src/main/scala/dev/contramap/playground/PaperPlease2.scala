package dev.contramap.playground

import PaperPlease2.Result._
import PaperPlease2.Document._
import PaperPlease2.Rule._
import java.security.KeyStore.Entry
import dev.contramap.playground.PaperPlease3.Rule.Always
import dev.contramap.playground.PaperPlease3.Rule.PassportNotExpired
import dev.contramap.playground.PaperPlease3.Rule.EntryPermitNotExpired
import dev.contramap.playground.PaperPlease3.Rule.CitizenPassport
import dev.contramap.playground.PaperPlease3.Rule.ForeignerPassport
import dev.contramap.playground.PaperPlease3.Rule.CorrelatePassportAndId
import dev.contramap.playground.PaperPlease3.Rule.CorrelatePassportAndPermit
import dev.contramap.playground.PaperPlease3.Rule.BothWith
import dev.contramap.playground.PaperPlease3.Rule.EitherWith
import dev.contramap.playground.PaperPlease3.Rule.ZipWith
import dev.contramap.playground.PaperPlease3.Rule.BothOp.Join
import dev.contramap.playground.PaperPlease3.Rule.BothOp.CopyRight
import dev.contramap.playground.PaperPlease3.Rule.BothOp.Identity

object PaperPlease2 {
  type Date = Long
  type UID = String
  type Identity = (String, String, Date) // (firstName, lastName, Date Of Birth)

  sealed trait Document
  object Document {
    final case class Passport(
        id: UID,
        identity: Identity,
        expiration: Date,
        foreign: Boolean
    ) extends Document

    final case class EntryPermit(id: UID, identity: Identity, expiration: Date)
        extends Document
    final case class IdCard(identity: Identity) extends Document
    final case class FingerPrints(data: String) extends Document
    final case class GrantOfAsylum(
        id: UID,
        identity: Identity,
        fingerPrints: FingerPrints
    ) extends Document
  }

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

    def unary_! : Result =
      self match {
        case Approved => Denied
        case _        => self
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
  }

  object Context {

    def citizen(now: Date)(passport: Passport, idCard: IdCard) =
      Context(now, Left((passport, Left(idCard))))

    def foreigner(now: Date)(passport: Passport, entryPermit: EntryPermit) =
      Context(now, Left((passport, Right(entryPermit))))

    def refugee(
        now: Date
    )(fingerPrints: FingerPrints, grantOfAsylum: GrantOfAsylum) =
      Context(now, Right((grantOfAsylum, fingerPrints)))
  }

  case class Rule[-A](run: Context[A] => Result) { self =>

    def unary_! : Rule[A] =
      Rule(ctx => !self.run(ctx))

    def &&[B](that: Rule[B]): Rule[(A, B)] =
      both(that)

    def both[B](that: Rule[B]): Rule[(A, B)] =
      bothWith(that)(identity)

    def join[A0 <: A](that: Rule[A0]): Rule[A0] =
      bothWith(that)(a => (a, a))

    def bothWith[B, C](that: Rule[B])(f: C => (A, B)): Rule[C] =
      Rule[C] { ctx =>
        val (a, b) = ctx.map(f).payload
        self.run(ctx.as(a)) && that.run(ctx.as(b))
      }

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

    def orElse[A0 <: A](that: Rule[A0]): Rule[A0] =
      zipWith(that)(_ || _)

    def zip[A0 <: A](that: Rule[A0]): Rule[A0] =
      zipWith(that)(_ && _)

    def zipWith[A0 <: A](
        that: Rule[A0]
    )(f: (Result, Result) => Result): Rule[A0] =
      Rule[A0] { ctx =>
        val r0 = self.run(ctx)
        val r1 = that.run(ctx)
        f(r0, r1)
      }
  }

  object Rule {
    type ||[A, B] = Either[A, B]

    val passportExpired: Rule[Passport] = Rule[Passport] {
      case Context(now, passport) =>
        if (passport.expiration < now) Denied else Approved
    }

    val entryPermitExpired: Rule[EntryPermit] = Rule[EntryPermit] {
      case Context(now, permit) =>
        if (permit.expiration < now) Denied else Approved
    }

    val always: Rule[Any] = Rule(_ => Approved)
    val hasIdCard: Rule[IdCard] = always
    val hasPassport: Rule[Passport] = always
    val hasEntryPermit: Rule[EntryPermit] = always

    val passportNotExpired: Rule[Passport] =
      hasPassport.join(!passportExpired)
    val entryPermitNotExpired: Rule[EntryPermit] =
      hasEntryPermit.join(!entryPermitExpired)

    val passportCorrelatesId: Rule[(Passport, IdCard)] = Rule { ctx =>
      val (passport, idCard) = ctx.payload
      if (passport.identity == idCard.identity) Approved else Detained
    }

    val passportCorrelatesEntryPermit: Rule[(Passport, EntryPermit)] = Rule {
      ctx =>
        val (passport, permit) = ctx.payload
        if (passport.id == permit.id && passport.identity == permit.identity)
          Approved
        else Detained
    }

    val citizenPassport: Rule[Passport] =
      Rule(ctx => if (ctx.payload.foreign) Denied else Approved)

    val foreignerPassport: Rule[Passport] =
      Rule(ctx => if (ctx.payload.foreign) Denied else Approved)

    val citizenRule: Rule[(Passport, IdCard)] =
      citizenPassport.join(passportNotExpired).bothWith(passportCorrelatesId) {
        case (passport, idCard) => (passport, (passport, idCard))
      }

    val foreignerRule: Rule[(Passport, EntryPermit)] =
      foreignerPassport
        .join(passportNotExpired)
        .bothWith(passportCorrelatesEntryPermit) {
          case (passport, permit) => (passport, (passport, permit))
        }

    val visitorRule: Rule[(Passport, IdCard || EntryPermit)] =
      passportCorrelatesId.eitherWith(passportCorrelatesEntryPermit) {
        case (passport, Left(idCard))  => Left((passport, idCard))
        case (passport, Right(permit)) => Right((passport, permit))
      }

    val refugeeRule: Rule[(GrantOfAsylum, FingerPrints)] = Rule { ctx =>
      val (grant, prints) = ctx.payload
      if (grant.fingerPrints.data == prints.data) Approved else Detained
    }

    val terroristAttack: Rule[Any] = Rule(_ => Aborted)

    type Visitor = (Passport, IdCard || EntryPermit)
    type Refugee = (GrantOfAsylum, FingerPrints)

    val game: Rule[Visitor || Refugee] =
      (visitorRule || refugeeRule).orElse(terroristAttack)

    case class Papers(
        passport: Option[Passport],
        idCard: Option[IdCard],
        fingerPrints: Option[FingerPrints],
        entryPermit: Option[EntryPermit],
        grantOfAsylum: Option[GrantOfAsylum]
    )

    def run(papers: Papers, now: Date): Result =
      papers match {
        case Papers(Some(passport), Some(idCard), _, _, _) =>
          game.run(Context.citizen(now)(passport, idCard))

        case Papers(Some(passport), _, _, Some(permit), _) =>
          game.run(Context.foreigner(now)(passport, permit))

        case Papers(_, _, Some(prints), _, Some(grant)) =>
          game.run(Context.refugee(now)(prints, grant))

        case _ => Denied
      }
  }
}

object PaperPlease3 {
  import PaperPlease2.Result
  import PaperPlease2.Context

  sealed trait Rule[-A] { self =>

    def &&[B](that: Rule[B]): Rule[(A, B)] =
      both(that)

    def both[B](that: Rule[B]): Rule[(A, B)] =
      bothWith(that)(Rule.BothOp.Identity)

    def join[A0 <: A](that: Rule[A0]): Rule[A0] =
      bothWith(that)(Rule.BothOp.Join)

    def bothWith[B, C](that: Rule[B])(op: Rule.BothOp): Rule[C] =
      Rule.BothWith(self, that, op)

    def ||[B](that: Rule[B]): Rule[Either[A, B]] =
      either(that)

    def either[B](that: Rule[B]): Rule[Either[A, B]] =
      eitherWith(that)(Rule.EitherOp.Identity)

    def eitherWith[B, C](that: Rule[B])(op: Rule.EitherOp): Rule[C] =
      Rule.EitherWith(self, that, op)

    def orElse[A0 <: A](that: Rule[A0]): Rule[A0] =
      zipWith(that)(Rule.ZipOp.Or)

    def zip[A0 <: A](that: Rule[A0]): Rule[A0] =
      zipWith(that)(Rule.ZipOp.And)

    def zipWith[A0 <: A](
        that: Rule[A0]
    )(op: Rule.ZipOp): Rule[A0] =
      Rule.ZipWith(self, that, op)
  }
  object Rule {
    type ||[A, B] = Either[A, B]

    case object PassportNotExpired extends Rule[Passport]
    case object EntryPermitNotExpired extends Rule[EntryPermit]
    
    case object CitizenPassport extends Rule[Passport]
    case object ForeignerPassport extends Rule[Passport]

    case object CorrelatePassportAndId extends Rule[(Passport, IdCard)]
    case object CorrelatePassportAndPermit extends Rule[(Passport, EntryPermit)]

    // case class Effect[A](f: Context[A] => Result) extends Rule[A]

    // val passportNotExpired: Rule[Passport] = Effect {
    //   case Context(now, passport) => ???
    // }
    
    case class Always(result: Result) extends Rule[Any]
    case class BothWith[A, B, C](left: Rule[A], right: Rule[B], op: BothOp) extends Rule[C]
    case class EitherWith[A, B, C](left: Rule[A], right: Rule[B], op: EitherOp) extends Rule[C]
    case class ZipWith[A, B](left: Rule[A], right: Rule[A], op: ZipOp) extends Rule[A]

    //case class Join2[A](left: Rule[A], right: Rule[A]) extends Rule[A]


    sealed trait BothOp
    object BothOp {
      case object CopyRight extends BothOp
      case object Join extends BothOp
      case object Identity extends BothOp
    }

    sealed trait EitherOp
    object EitherOp {
      case object CopyLeft extends EitherOp
      case object Identity extends EitherOp
    }

    sealed trait ZipOp
    object ZipOp {
      case object And extends ZipOp
      case object Or extends ZipOp
    }

    val citizenPassport: Rule[Passport] =
      CitizenPassport.bothWith(PassportNotExpired)(BothOp.Join)

    val citizen: Rule[(Passport, IdCard)] =
      citizenPassport.bothWith(CorrelatePassportAndId)(BothOp.CopyRight)

    val foreignerPassport: Rule[Passport] =
      ForeignerPassport.bothWith(PassportNotExpired)(BothOp.Join)

    val foreigner: Rule[(Passport, EntryPermit)] =
      foreignerPassport.eitherWith(CorrelatePassportAndPermit)(
        EitherOp.CopyLeft
      )

    val visitor: Rule[(Passport, IdCard || EntryPermit)] =
      citizen.eitherWith(foreigner)(EitherOp.CopyLeft)

    val terroristAttack: Rule[Any] = Always(Result.Aborted)

    val game = visitor.orElse(terroristAttack)

    def run[A](rule: Rule[A])(ctx: Context[A]): Result =
      (ctx.payload, rule) match {
        case (_, Always(result)) => result
        case (passport, PassportNotExpired) =>
          if (passport.expiration <= ctx.now) Approved else Denied
        case (permit, EntryPermitNotExpired) =>
          if (permit.expiration <= ctx.now) Approved else Denied
        case (passport, CitizenPassport) =>
          if (passport.foreign) Denied else Approved
        case (passport, ForeignerPassport) =>
          if (passport.foreign) Approved else Denied
        case ((passport: Passport, idCard: IdCard), CorrelatePassportAndId) =>
          if (passport.identity == idCard.identity) Approved else Detained
        case (
              (passport: Passport, permit: EntryPermit),
              CorrelatePassportAndPermit
            ) =>
          if (passport.identity == permit.identity && passport.id == permit.id)
            Approved
          else Detained

        case (payload, bw: BothWith[a, b, A]) =>
          val (a, b) = evalBothWithOp[a, b, A](bw.op)(payload)
          run(bw.left)(ctx.as(a)) && run(bw.right)(ctx.as(b))

        case (payload, EitherWith(left, right, op)) =>
          evalEitherWithOp(op)(payload) match {
            case Left(value)  => run(left)(value)
            case Right(value) => run(right)(value)
          }

        case (payload, ZipWith(left, right, op)) => 
          
      }

// Op is too specific, there are two dsl's here
    def evalBothWithOp[A, B, C](op: BothOp): C => (A, B) =
      op match {
        case Join      => a => (a, a)
        case CopyRight => (a, b) => (a, (a, b))
        case Identity  => identity
      }

    def evalEitherWithOp[A, B, C](op: EitherOp): C => Either[A, B] = ???
    def evalZipWithOp[A, B, C](op: ZipWithOp): C => Either[A, B] = ???
  }
}
