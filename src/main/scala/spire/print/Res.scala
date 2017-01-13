package spire.print

import spire.print.PrettyStringBuilder.Pos
import spire.util.Opt

// Implements unparsing of expressions according to
// N. Ramsey, “Unparsing expressions with prefix and postfix operators,”
// Software: Practice and Experience, vol. 28, no. 12, pp. 1327–1356, 1998.

sealed abstract class Side

object Side {
  case object Left extends Side
  case object Right extends Side
}

import Side.{Left, Right}

sealed abstract class Res

case object Atom extends Res {

  def apply(b: Boolean)(implicit sb: PrettyStringBuilder): Res = { sb.append(b); Atom }
  def apply(c: Char)(implicit sb: PrettyStringBuilder): Res = { sb.append(c); Atom }
  def apply(str: Array[Char])(implicit sb: PrettyStringBuilder): Res = { sb.append(str); Atom }
  def apply(str: Array[Char], offset: Int, len: Int)(implicit sb: PrettyStringBuilder): Res = { sb.append(str, offset, len); Atom }
  def apply(s: CharSequence)(implicit sb: PrettyStringBuilder): Res = { sb.append(s); Atom }
  def apply(s: CharSequence, start: Int, end: Int)(implicit sb: PrettyStringBuilder): Res = { sb.append(s, start, end); Atom }
  def apply(d: Double)(implicit sb: PrettyStringBuilder): Res = { sb.append(d); Atom }
  def apply(f: Float)(implicit sb: PrettyStringBuilder): Res = { sb.append(f); Atom }
  def apply(i: Int)(implicit sb: PrettyStringBuilder): Res = { sb.append(i); Atom }
  def apply(l: Long)(implicit sb: PrettyStringBuilder): Res = { sb.append(l); Atom }
  def apply(obj: AnyRef)(implicit sb: PrettyStringBuilder): Res = { sb.append(obj); Atom }
  def apply(str: String)(implicit sb: PrettyStringBuilder): Res = { sb.append(str); Atom }
  def apply(sb: StringBuffer)(implicit psb: PrettyStringBuilder): Res = { psb.append(sb); Atom }

}

sealed abstract class Op extends Res {
  def symbol: String
  def priority: Int
}

object Op {

  def unapply(op: Op): Opt[(String, Int)] = Opt((op.symbol, op.priority))

  case class EnterPos(val i: Int) extends AnyVal

  object EnterPos {
    implicit def toPos(e: EnterPos): Pos = Pos(e.i)
    implicit def fromPos(p: Pos): EnterPos = EnterPos(p.i)
  }

  sealed trait UnaryOp extends Op {

    def needsParens(inner: Res): Boolean

    def enter(pos: Pos): EnterPos = pos

    def enter(implicit s: PrettyStringBuilder): EnterPos = s.currentPos

    def exit(e: EnterPos, inner: Res)(implicit s: PrettyStringBuilder): Unit =
      if (needsParens(inner)) {
        s.addFixup(e, "(")
        s.append(")")
      }

  }

  // corresponds to fixity=PREFIX
  case class Prefix(symbol: String, priority: Int) extends UnaryOp { outerOp =>

    def apply[A:PrettyPrint](a: A)(implicit sb: PrettyStringBuilder): Res = {
      sb.append(outerOp.symbol)
      val innerStart = sb.currentPos
      implicitly[PrettyPrint[A]].print(a) match {
        case innerOp: Op if needsParens(innerOp) =>
          sb.addFixup(innerStart, "(")
          sb.append(")")
        case _ =>
      }
      outerOp
    }

    def needsParens(inner: Res): Boolean = inner match {
      case Atom => false
      case innerOp: Op if innerOp.priority < outerOp.priority => false // inner operator has stronger coupling
      case _: Op.Prefix => false // case (c), both operators have the same fixity
      case _ => true
    }

  }
  // corresponds to fixity=POSTFIX
  case class Postfix(symbol: String, priority: Int) extends UnaryOp { outerOp =>

    def apply[A:PrettyPrint](a: A)(implicit sb: PrettyStringBuilder): Res = {
      val innerStart = sb.currentPos
      implicitly[PrettyPrint[A]].print(a) match {
        case innerOp: Op if needsParens(innerOp) =>
          sb.addFixup(innerStart, "(")
          sb.append(")")
        case _ =>
      }
      sb.append(outerOp.symbol)
      outerOp
    }

    def needsParens(inner: Res): Boolean = inner match {
      case Atom => false
      case innerOp: Op if innerOp.priority < outerOp.priority => false // inner operator has stronger coupling
      case _: Op.Postfix => false // case (c), both operators have the same fixity
      case _ => true
    }

  }

  case class LeftEnterPos(val i: Int) extends AnyVal

  object LeftEnterPos {
    implicit def toPos(e: LeftEnterPos): Pos = Pos(e.i)
    implicit def fromPos(p: Pos): LeftEnterPos = LeftEnterPos(p.i)
  }

  case class RightEnterPos(val i: Int) extends AnyVal

  object RightEnterPos {
    implicit def toPos(e: RightEnterPos): Pos = Pos(e.i)
    implicit def fromPos(p: Pos): RightEnterPos = RightEnterPos(p.i)
  }


  /** Infix operator, corresponds to fixity = INFIX of ...
    *
    * For associativity = Opt(Left), it is INFIX of LEFT
    *                   = Opt(Right), it is INFIX of RIGHT
    *                   = Opt.empty[Side], it is INFIX of NONASSOC
    */
  case class Infix(symbol: String, priority: Int, associativity: Opt[Side]) extends Op { outerOp =>

    def apply[L:PrettyPrint, R:PrettyPrint](l: L, r: R)(implicit sb: PrettyStringBuilder): Res = {
      val leftStart = sb.currentPos
      implicitly[PrettyPrint[L]].print(l) match {
        case innerOp: Op if needsParens(innerOp, Side.Left) =>
          sb.addFixup(leftStart, "(")
          sb.append(")")
        case _ =>
      }
      sb.append(outerOp.symbol)
      val rightStart = sb.currentPos
      implicitly[PrettyPrint[R]].print(r) match {
        case innerOp: Op if needsParens(innerOp, Side.Right) =>
          sb.addFixup(rightStart, "(")
          sb.append(")")
        case _ =>
      }
      outerOp
    }

    def needsParens(inner: Res, side: Side): Boolean = inner match {
      case Atom => false
      case innerOp: Op if innerOp.priority < outerOp.priority => false // inner operator has stronger coupling
      case _: Op.Postfix if side == Side.Left => false // (... postfix) ...
      case _: Op.Prefix if side == Side.Right => false // // ... (prefix ...)
      case innerOp@Op.Infix(_, _, Opt(Side.Left)) if side == Side.Left =>
        // (... infixI ...) infixO ...
        // if infixI and infixO are both left associative, with same priority => no parentheses needed.
        // otherwise, parentheses required
        (outerOp.priority != innerOp.priority) || (outerOp.associativity != Opt(Side.Left))
      case innerOp@Op.Infix(_, _, Opt(Side.Right)) if side == Side.Right =>
        // ... infixO (... infixI ...)
        // if infixI and infixO are both right associative, with the same priority => no parentheses needed
        (outerOp.priority != innerOp.priority) || (outerOp.associativity != Opt(Side.Right))
      case _ => true
    }

    def enterLeft(pos: Pos): LeftEnterPos = pos
    def enterLeft(implicit s: PrettyStringBuilder): LeftEnterPos = s.currentPos

    def exitLeft(e: LeftEnterPos, inner: Res)(implicit s: PrettyStringBuilder): Unit =
      if (needsParens(inner, Left)) {
        s.addFixup(e, "(")
        s.append(")")
      }

    def enterRight(pos: Pos): LeftEnterPos = pos
    def enterRight(implicit s: PrettyStringBuilder): RightEnterPos = s.currentPos

    def exitRight(e: RightEnterPos, inner: Res)(implicit s: PrettyStringBuilder): Unit =
      if (needsParens(inner, Right)) {
        s.addFixup(e, "(")
        s.append(")")
      }

  }

  object Infix {
    def leftAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt(Left))
    def rightAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt(Right))
    def nonAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt.empty[Side])
  }

}
