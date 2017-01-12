package spire.print

import spire.util.Opt

// Implements unparsing of expressions according to
// N. Ramsey, “Unparsing expressions with prefix and postfix operators,”
// Software: Practice and Experience, vol. 28, no. 12, pp. 1327–1356, 1998.

trait PrintHelpers {

  import ParenPrint.noParens

  // TODO: link all "append" FixupStringBuilder methods

  def atom(str: String)(implicit sb: FixupStringBuilder): Res = {
    sb.append(str)
    Atom
  }

  def atom(i: Int)(implicit sb: FixupStringBuilder): Res = {
    sb.append(i)
    Atom
  }

  def atom(l: Long)(implicit sb: FixupStringBuilder): Res = {
    sb.append(l)
    Atom
  }

  def prefix[A:ParenPrint](outerOp: Op.Prefix, a: A)(implicit sb: FixupStringBuilder): Res = {
    sb.append(outerOp.symbol)
    val innerStart = sb.length
    implicitly[ParenPrint[A]].printSb(a) match {
      case innerOp: Op if !noParens(outerOp, innerOp, Opt.empty[Side]) =>
        sb.addFixup(innerStart, "(")
        sb.append(")")
      case _ =>
    }
    outerOp
  }

  def postfix[A:ParenPrint](a: A, outerOp: Op.Postfix)(implicit sb: FixupStringBuilder): Res = {
    val innerStart = sb.length
    implicitly[ParenPrint[A]].printSb(a) match {
      case innerOp: Op if !noParens(outerOp, innerOp, Opt.empty[Side]) =>
        sb.addFixup(innerStart, "(")
        sb.append(")")
      case _ =>
    }
    sb.append(outerOp.symbol)
    outerOp
  }


  def infix[L:ParenPrint, R:ParenPrint](l: L, outerOp: Op.Infix, r: R)(implicit sb: FixupStringBuilder): Res = {
    val leftStart = sb.length
    implicitly[ParenPrint[L]].printSb(l) match {
      case innerOp: Op if !noParens(outerOp, innerOp, Opt(Side.Left)) =>
        sb.addFixup(leftStart, "(")
        sb.append(")")
      case _ =>
    }
    sb.append(outerOp.symbol)
    val rightStart = sb.length
    implicitly[ParenPrint[R]].printSb(r) match {
      case innerOp: Op if !noParens(outerOp, innerOp, Opt(Side.Right)) =>
        sb.addFixup(rightStart, "(")
        sb.append(")")
      case inner =>
    }
    outerOp
  }

}

trait ParenPrint[-A] extends PrintHelpers {

  def print(a: A): (String, Res)

  def printSb(a: A)(implicit s: FixupStringBuilder): Res = {
    val (string, res) = print(a)
    s.append(string)
    res
  }

}

object ParenPrint {

  def apply[A](a: A)(implicit ev: ParenPrint[A]): String = {
    implicit val sb = FixupStringBuilder.newBuilder()
    ev.printSb(a)
    sb.toString
  }

  /** Returns true if the inner expression should not be parenthesised when appearing on the
    * given side of the outer expression.
    */
  def noParens(outer: Op, inner: Op, sideOfInnerInfix: Opt[Side]): Boolean = {
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

  def atom(str: String): (String, Res) = (str, Atom)

  def prefix[A:ParenPrint](outerOp: Op.Prefix, a: A): (String, Res) =
    implicitly[ParenPrint[A]].print(a) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt.empty[Side]) => (outerOp.symbol + "(" + inner + ")", outerOp)
      case (inner, _) => (outerOp.symbol + inner, outerOp)
    }

  def postfix[A:ParenPrint](a: A, outerOp: Op.Postfix): (String, Res) =
    implicitly[ParenPrint[A]].print(a) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt.empty[Side]) => ("(" + inner + ")" + outerOp.symbol, outerOp)
      case (inner, _) => (inner + outerOp.symbol, outerOp)
    }

  def infix[L:ParenPrint, R:ParenPrint](l: L, outerOp: Op.Infix, r: R): (String, Res) = {
    val leftStr = implicitly[ParenPrint[L]].print(l) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt(Side.Left)) => "(" + inner + ")"
      case (inner, _) => inner
    }
    val rightStr = implicitly[ParenPrint[R]].print(r) match {
      case (inner, innerOp: Op) if !noParens(outerOp, innerOp, Opt(Side.Right)) => "(" + inner + ")"
      case (inner, _) => inner
    }
    (leftStr + outerOp.symbol + rightStr, outerOp)
  }

}
