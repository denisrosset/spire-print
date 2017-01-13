package spire.print

import scala.annotation.tailrec

import spire.algebra._
import spire.math._
import spire.math.poly.Term
import spire.print.Op.{Infix, Prefix}
import spire.print.PrettyStringBuilder.Pos
import spire.syntax.cfor._
import spire.util.Opt

trait PrettyPrintable {

  override def toString = {
    implicit val pp: PrettyPrint[this.type] = defaultPrettyPrint
    PrettyPrint.pretty[this.type](this)(pp.asInstanceOf[PrettyPrint[this.type]])
  }

  def defaultPrettyPrint: PrettyPrint[this.type]

}

// Implements unparsing of expressions according to
// N. Ramsey, “Unparsing expressions with prefix and postfix operators,”
// Software: Practice and Experience, vol. 28, no. 12, pp. 1327–1356, 1998.

trait PrettyPrint[-A] {

  def print(a: A)(implicit s: PrettyStringBuilder): Res

}

object PrettyPrint {

  val `unaryop_-` = Prefix("-", 10)
  val `op_^` = Infix.rightAssoc("*", 20)
  val `invop_*` = Infix.leftAssoc("", 30)
  val `op_*` = Infix.leftAssoc("*", 30)
  val `op_/` = Infix.leftAssoc("/", 30)
  val `op_+` = Infix.leftAssoc("+", 40)
  val `op_-` = Infix.leftAssoc("-", 40)
  val `op_,` = Infix.leftAssoc(",", 100) // lowest priority


  def pretty[A](a: A)(implicit pp: PrettyPrint[A]): String = {
    implicit val sb = PrettyStringBuilder.newBuilder()
    pp.print(a)
    sb.toString
  }

  def apply[A](implicit ev: PrettyPrint[A]): PrettyPrint[A] = ev

  case class Verbatim(val string: String) extends AnyVal

  object default extends PrettyPrint[Any] {

    val Word = """\w+""".r

    def print(a: Any)(implicit sb: PrettyStringBuilder): Res = a match {
      case pp: PrettyPrintable => pp.defaultPrettyPrint.print(pp)
      case s: String => string.print(s)
      case v: Verbatim => _verbatim.print(v)
      case i: Int => int.print(i)
      case l: Long => long.print(l)
      case _ => a.toString match {
        case str@Word() =>
          sb.append(str)
          Atom
        case str =>
          sb.append("(")
          sb.append(str)
          sb.append(")")
          Atom
      }
    }

  }

  // TODO: implement all primitive types

  implicit object string extends PrettyPrint[String] {

    def print(str: String)(implicit s: PrettyStringBuilder): Res = {
      // inspired by http://stackoverflow.com/a/40073137
      s.append("\"") // quote
      cforRange(0 until str.length) { i =>
        str(i) match {
          case '\b' => s.append("\\b")
          case '\t' => s.append("\\t")
          case '\n' => s.append("\\n")
          case '\f' => s.append("\\f")
          case '\r' => s.append("\\r")
          case '"'  => s.append("\\\"")
          case '\'' => s.append("\\\'")
          case '\\' => s.append("\\\\")
          case ch if ch.isControl =>
            s.append("\\0")
            s.append(Integer.toOctalString(ch.toInt))
          case ch => s.append(ch)
        }
      }
      s.append("\"")
      Atom
    }

  }

  implicit object _verbatim extends PrettyPrint[Verbatim] {

    def print(a: Verbatim)(implicit s: PrettyStringBuilder): Res = Atom(a.string)

  }

  implicit object int extends PrettyPrint[Int] {

    def print(i: Int)(implicit s: PrettyStringBuilder): Res = {
      Atom(i)
      if (i < 0) `unaryop_-` else Atom
    }

  }

  implicit object long extends PrettyPrint[Long] {

    def print(l: Long)(implicit s: PrettyStringBuilder): Res = {
      Atom(l)
      if (l < 0) `unaryop_-` else Atom
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

  val verbatimInf = Verbatim("∞")
  val verbatimMinusInf = Verbatim("-∞")

  implicit def interval[A:PrettyPrint]: PrettyPrint[Interval[A]] = new PrettyPrint[Interval[A]] {
    // copy/paste from Interval
    @inline protected[this] final def isClosed(flags: Int): Boolean = flags == 0
    @inline protected[this] final def isClosedLower(flags: Int): Boolean = (flags & 1) == 0
    @inline protected[this] final def isClosedUpper(flags: Int): Boolean = (flags & 2) == 0

    @inline protected[this] final def isOpen(flags: Int): Boolean = flags == 3
    @inline protected[this] final def isOpenLower(flags: Int): Boolean = (flags & 1) == 1
    @inline protected[this] final def isOpenUpper(flags: Int): Boolean = (flags & 2) == 2

    def print(int: Interval[A])(implicit s: PrettyStringBuilder): Res = int match {
      case All() => Atom("(-∞, ∞)")
      case Empty() => Atom("(Ø)")
      case Above(lower, flags) =>
        if (isClosedLower(flags)) s.append("[") else s.append("(")
        `op_,`(lower, verbatimInf)
        s.append(")")
        Atom // result is enclosed by [/( ) and thus an atom
      case Below(upper, flags) =>
        s.append("(")
        `op_,`(verbatimMinusInf, upper)
        if (isClosedUpper(flags)) s.append("]") else s.append(")")
        Atom // result is enclosed by [ )/] and thus an atom
      case Point(p) =>
        s.append("[")
        PrettyPrint[A].print(p)
        s.append("]")
        Atom // result is enclosed by [ ] and thus an atom
      case Bounded(lower, upper, flags) =>
        if (isClosedLower(flags)) s.append("[") else s.append("(")
        `op_,`(lower, upper)
        if (isClosedUpper(flags)) s.append("]") else s.append(")")
        Atom // result is enclosed
    }

  }

  implicit def polynomial[A:Eq:NegativeFirstTerm:Ring:PrettyPrint]: PrettyPrint[Polynomial[A]] = new PrettyPrint[Polynomial[A]] {

    def print(poly: Polynomial[A])(implicit s: PrettyStringBuilder): Res =
      if (poly.isConstant) PrettyPrint[A].print(poly.nth(0)) else {
        def printX(exp: Int): Res = exp match {
          case 0 => Atom("1")
          case 1 => Atom("x")
          case e =>
            s.append("x^")
            s.append(exp)
            `op_^`
        }

        def printTerm(coeff: A, exp: Int): Res =
          if (exp == 0) PrettyPrint[A].print(coeff)
          else if (Ring[A].isOne(coeff)) printX(exp)
          else {
            val lp = `invop_*`.enterLeft
            val lr = PrettyPrint[A].print(coeff)
            `invop_*`.exitLeft(lp, lr)
            val rp = `invop_*`.enterRight
            val rr = printX(exp)
            `invop_*`.exitRight(rp, rr)
            `invop_*`
          }

        val it = poly.termsIterator

        val firstPos = s.currentPos
        val firstRes = {
          val Term(coeff, exp) = it.next()
          printTerm(coeff, exp)
        }

        def iter(it: Iterator[Term[A]], leftPos: Pos, leftRes: Res): Res =
          if (!it.hasNext) leftRes else {
            val Term(coeff, exp) = it.next()
            if (NegativeFirstTerm[A].apply(coeff)) {
              val lp = `op_-`.enterLeft(leftPos)
              `op_-`.exitLeft(lp, leftRes)
              `op_-`.printOp()
              val rp = `op_-`.enterRight
              val rr = printTerm(Ring[A].negate(coeff), exp)
              `op_-`.exitRight(rp, rr)
              iter(it, leftPos, `op_-`)
            } else {
              val lp = `op_+`.enterLeft(leftPos)
              `op_+`.exitLeft(lp, leftRes)
              `op_+`.printOp()
              val rp = `op_+`.enterRight
              val rr = printTerm(coeff, exp)
              `op_+`.exitRight(rp, rr)
              iter(it, leftPos, `op_+`)
            }
          }
        iter(it, firstPos, firstRes)
      }

  }

}
