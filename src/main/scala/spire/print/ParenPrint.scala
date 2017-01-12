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

case object Atom extends Res

sealed abstract class Op extends Res {
  def symbol: String
  def priority: Int
}

object Op {

  def unapply(op: Op): Opt[(String, Int)] = Opt((op.symbol, op.priority))

  // corresponds to fixity=PREFIX
  case class Prefix(symbol: String, priority: Int) extends Op
  // corresponds to fixity=POSTFIX
  case class Postfix(symbol: String, priority: Int) extends Op

  /** Infix operator, corresponds to fixity = INFIX of ...
    *
    * For associativity = Opt(Left), it is INFIX of LEFT
    *                   = Opt(Right), it is INFIX of RIGHT
    *                   = Opt.empty[Side], it is INFIX of NONASSOC
    */
  case class Infix(symbol: String, priority: Int, associativity: Opt[Side]) extends Op

  object Infix {
    def ofLeft(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt(Left))
    def ofRight(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt(Right))
    def ofNonAssoc(symbol: String, priority: Int): Infix = Infix(symbol, priority, Opt.empty[Side])
  }

}

trait ParenPrint[-A] {

  def print(a: A): (String, Res)

}

object ParenPrint {

  /** Returns true if the inner expression should not be parenthesised when appearing on the
    * given side of the outer expression.
    */
  def noParens(outer: Op, inner: Op, sideOfInnerInfix: Opt[Side]): Boolean = {
    if (outer.isInstanceOf[Op.Prefix] || outer.isInstanceOf[Op.Postfix]) require(sideOfInnerInfix.isEmpty)
    if (outer.isInstanceOf[Op.Infix]) require(sideOfInnerInfix.nonEmpty)
    (outer, inner, sideOfInnerInfix) match {
      // rules, page 1342 of [Ramsey1998]
      case (Op(_, po), Op(_, pi), _) if pi < po => true // inner side has stronger coupling than outer side
      case (_, _: Op.Postfix, Opt(Left))  => true // (... postfix) ...
      case (_, _: Op.Prefix, Opt(Right)) => true // ... (prefix ...)
      case (Op.Infix(_, po, fo), Op.Infix(_, pi, Opt(Left)), Opt(Left)) =>
        // (... infixI ...) infixO ...
        // if infixI and infixO are both left associative, with same priority => no parenthesis needed
        // otherwise required
        (po == pi) && (fo == Opt(Left))
      case (Op.Infix(_, po, fo), Op.Infix(_, pi, Opt(Right)), Opt(Right)) =>
        // ... infixO (... infixI ...)
        // if infixI and infixO are both right associative, with the same priority => no parenthesis needed
        (po == pi) && (fo == Opt(Right))

        // case (c) is the paper, both operators have the same fixity
      case (_: Op.Postfix, _: Op.Postfix, _) => true
      case (_: Op.Prefix, _: Op.Prefix, _) => true
      case _ => false
    }
  }

  def atom(str: String): (String, Res) = (str, Atom)

  def prefix[A:ParenPrint](outerOp: Op.Prefix, a: A): (String, Res) =
    ParenPrint[A].print(a) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt.empty[Side]) => (outerOp.symbol + "(" + inner + ")", outerOp)
      case (inner, _) => (outerOp.symbol + inner, outerOp)
    }

  def postfix[A:ParenPrint](a: A, outerOp: Op.Postfix): (String, Res) =
    ParenPrint[A].print(a) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt.empty[Side]) => ("(" + inner + ")" + outerOp.symbol, outerOp)
      case (inner, _) => (inner + outerOp.symbol, outerOp)
    }

  def infix[L:ParenPrint, R:ParenPrint](l: L, outerOp: Op.Infix, r: R): (String, Res) = {
    val leftStr = ParenPrint[L].print(l) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt(Left)) => "(" + inner + ")"
      case (inner, _) => inner
    }
    val rightStr = ParenPrint[R].print(r) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt(Right)) => "(" + inner + ")"
      case (inner, _) => inner
    }
    (leftStr + outerOp.symbol + rightStr, outerOp)
  }

  @inline final def apply[A](implicit ev: ParenPrint[A]): ParenPrint[A] = ev

}
