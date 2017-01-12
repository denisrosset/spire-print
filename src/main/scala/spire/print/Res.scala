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

