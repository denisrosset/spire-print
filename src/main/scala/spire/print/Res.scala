package spire.print

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

  // corresponds to fixity=PREFIX
  case class Prefix(symbol: String, priority: Int) extends Op { outerOp =>

    def apply[A:PrettyPrint](a: A)(implicit sb: PrettyStringBuilder): Res = {
      sb.append(outerOp.symbol)
      val innerStart = sb.length
      implicitly[PrettyPrint[A]].print(a) match {
        case innerOp: Op if !PrettyPrint.noParens(outerOp, innerOp, Opt.empty[Side]) =>
          sb.addFixup(innerStart, "(")
          sb.append(")")
        case _ =>
      }
      outerOp
    }

  }
  // corresponds to fixity=POSTFIX
  case class Postfix(symbol: String, priority: Int) extends Op { outerOp =>

    def apply[A:PrettyPrint](a: A)(implicit sb: PrettyStringBuilder): Res = {
      val innerStart = sb.length
      implicitly[PrettyPrint[A]].print(a) match {
        case innerOp: Op if !PrettyPrint.noParens(outerOp, innerOp, Opt.empty[Side]) =>
          sb.addFixup(innerStart, "(")
          sb.append(")")
        case _ =>
      }
      sb.append(outerOp.symbol)
      outerOp
    }

  }

  /** Infix operator, corresponds to fixity = INFIX of ...
    *
    * For associativity = Opt(Left), it is INFIX of LEFT
    *                   = Opt(Right), it is INFIX of RIGHT
    *                   = Opt.empty[Side], it is INFIX of NONASSOC
    */
  case class Infix(symbol: String, priority: Int, associativity: Opt[Side]) extends Op { outerOp =>

    def apply[L:PrettyPrint, R:PrettyPrint](l: L, r: R)(implicit sb: PrettyStringBuilder): Res = {
      val leftStart = sb.length
      implicitly[PrettyPrint[L]].print(l) match {
        case innerOp: Op if !PrettyPrint.noParens(outerOp, innerOp, Opt(Side.Left)) =>
          sb.addFixup(leftStart, "(")
          sb.append(")")
        case _ =>
      }
      sb.append(outerOp.symbol)
      val rightStart = sb.length
      implicitly[PrettyPrint[R]].print(r) match {
        case innerOp: Op if !PrettyPrint.noParens(outerOp, innerOp, Opt(Side.Right)) =>
          sb.addFixup(rightStart, "(")
          sb.append(")")
        case _ =>
      }
      outerOp
    }

  }

  object Infix {
    def leftAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt(Left))
    def rightAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt(Right))
    def nonAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt.empty[Side])
  }

}
