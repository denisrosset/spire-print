package spire.print

import spire.algebra.{IsReal, Ring, Signed}
import spire.math.{Complex, Rational, SafeLong}
import spire.print.Op.{Infix, Prefix}
import spire.util.Opt

// Implements unparsing of expressions according to
// N. Ramsey, “Unparsing expressions with prefix and postfix operators,”
// Software: Practice and Experience, vol. 28, no. 12, pp. 1327–1356, 1998.

trait PrettyPrint[-A] {

  def print(a: A)(implicit s: PrettyStringBuilder): Res

}

object PrettyPrint {

  val `unaryop_-` = Prefix("-", 10)
  val `invop_*` = Infix.leftAssoc("", 30)
  val `op_*` = Infix.leftAssoc("*", 30)
  val `op_/` = Infix.leftAssoc("/", 30)
  val `op_+` = Infix.leftAssoc("+", 40)
  val `op_-` = Infix.leftAssoc("-", 40)


  def string[A](a: A)(implicit pp: PrettyPrint[A]): String = {
    implicit val sb = PrettyStringBuilder.newBuilder()
    pp.print(a)
    sb.toString
  }

  def apply[A](implicit ev: PrettyPrint[A]): PrettyPrint[A] = ev

  /** Returns true if the inner expression should not be parenthesised when appearing on the
    * given side of the outer expression.
    */
  protected[print] def noParens(outer: Op, inner: Op, sideOfInnerInfix: Opt[Side]): Boolean = {
    if (outer.isInstanceOf[Op.Prefix] || outer.isInstanceOf[Op.Postfix]) require(sideOfInnerInfix.isEmpty)
    if (outer.isInstanceOf[Op.Infix]) require(sideOfInnerInfix.nonEmpty)
    (outer, inner, sideOfInnerInfix) match {
      // rules, page 1342 of [Ramsey1998]
      case (Op(_, po), Op(_, pi), _) if pi < po => true // inner side has stronger coupling than outer side
      case (_, _: Op.Postfix, Opt(Side.Left))  => true // (... postfix) ...
      case (_, _: Op.Prefix, Opt(Side.Right)) => true // ... (prefix ...)
      case (Op.Infix(_, po, fo), Op.Infix(_, pi, Opt(Side.Left)), Opt(Side.Left)) =>
        // (... infixI ...) infixO ...
        // if infixI and infixO are both left associative, with same priority => no parenthesis needed
        // otherwise required
        (po == pi) && (fo == Opt(Side.Left))
      case (Op.Infix(_, po, fo), Op.Infix(_, pi, Opt(Side.Right)), Opt(Side.Right)) =>
        // ... infixO (... infixI ...)
        // if infixI and infixO are both right associative, with the same priority => no parenthesis needed
        (po == pi) && (fo == Opt(Side.Right))

        // case (c) is the paper, both operators have the same fixity
      case (_: Op.Postfix, _: Op.Postfix, _) => true
      case (_: Op.Prefix, _: Op.Prefix, _) => true
      case _ => false
    }
  }

  implicit object safeLong extends PrettyPrint[SafeLong] {

    def print(a: SafeLong)(implicit s: PrettyStringBuilder): Res = {
      if (a.isValidLong)
        Atom(a.toLong)
      else
        Atom(a.toString)
      if (a.signum < 0) `unaryop_-` else Atom
    }

  }

  implicit object rational extends PrettyPrint[Rational] {

    def print(a: Rational)(implicit s: PrettyStringBuilder): Res = {
      if (a.isWhole) {
        if (a.isValidLong) Atom(a.numeratorAsLong) else Atom(a.toString)
        if (a.signum < 0) `unaryop_-` else Atom
      } else `op_/`(a.numerator, a.denominator)
    }

  }

  implicit def complex[A:PrettyPrint:IsReal:Ring]: PrettyPrint[Complex[A]] = new PrettyPrint[Complex[A]] {

    def print(a: Complex[A])(implicit s: PrettyStringBuilder): Res = {
      if (a.isReal) PrettyPrint[A].print(a.real)
      else if (a.isImaginary) {
        val res = PrettyPrint[A].print(a.imag)
        s.append("i")
        res
      } else if (Signed[A].isSignNegative(a.imag)) {
        val res = `op_-`(a.real, Ring[A].negate(a.imag))
        s.append("i")
        res
      } else {
        val res = `op_+`(a.real, a.imag)
        s.append("i")
        res
      }
    }

  }

}
