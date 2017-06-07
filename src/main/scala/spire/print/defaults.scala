package spire.print

import spire.algebra._
import spire.math.poly.Term
import spire.math._
import spire.print.Op.{Infix, Prefix}
import spire.print.PrettyStringBuilder.Pos
import spire.syntax.cfor.cforRange
import fastparse.{WhitespaceApi, noApi}
import fastparse.noApi._
import spire.std.any._

object whitespace {
  val Whitespace = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(CharsWhile(" \n\t".contains(_)).?)
  }
}

object defaults {

  import whitespace.Whitespace._

  abstract class IntegerParser[A:AdditiveGroup] extends AdditiveGroupParser[A] {
    /** Parses a series of digits into an integer */
    def fromString(s: String): A
    val signlessZero: P[A] = P( "0".! ).map(x => AdditiveGroup[A].zero)
    val signlessPositive: P[A] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(fromString)
    val signlessParser: P[A] = P(signlessZero | signlessPositive).opaque("<nonneg-integer>")
    val nonNegative: P[A] = P("+".? ~ signlessParser)
    // negative integers
    val negative: P[A] = P( "-" ~ signlessParser ).map(AdditiveGroup[A].negate)
    // generic parser
    val parser: P[A] = P( negative | nonNegative ).opaque("<integer>")
  }

  abstract class PrimitiveIntegerParser[A:AdditiveGroup:NumberTag:IsIntegral] extends AdditiveGroupParser[A] {
    require(NumberTag[A].hasMinValue.isDefined)
    /** Parses a series of digits into an integer */
    def fromString(s: String): A
    protected val minValue: A = NumberTag[A].hasMinValue.get
    protected val minValueDigits: String = (-IsIntegral[A].toBigInt(minValue)).toString
    protected val minValueParser: P[A] = P("-" ~ minValueDigits).map(x => minValue)
    val signlessZero: P[A] = P( "0".! ).map(x => AdditiveGroup[A].zero)
    val signlessPositive: P[A] = P( (CharIn('1'to'9') ~~ CharIn('0'to'9').repX).! ).map(fromString)
    val signlessParser: P[A] = P(signlessZero | signlessPositive).opaque("<nonneg-integer>")
    val nonNegative: P[A] = P("+".? ~ signlessParser)
    // negative integers
    protected val notMinValueNegative: P[A] = P( "-" ~ signlessParser ).map(AdditiveGroup[A].negate)
    val negative: P[A] = P( minValueParser | notMinValueNegative )
    // generic parser
    val parser: P[A] = P( negative | nonNegative ).opaque("<integer>")
  }

  implicit object byteParser extends PrimitiveIntegerParser[Byte] {
    def fromString(s: String): Byte = s.toByte
  }

  implicit object shortParser extends PrimitiveIntegerParser[Short] {
    def fromString(s: String): Short = s.toShort
  }

  implicit object intParser extends PrimitiveIntegerParser[Int] {
    def fromString(s: String): Int = s.toInt
  }

  implicit object longParser extends PrimitiveIntegerParser[Long] {
    def fromString(s: String): Long = s.toLong
  }

  implicit object safeLongParser extends IntegerParser[SafeLong] {
    def fromString(s: String): SafeLong = SafeLong(BigInt(s))
  }

  implicit object rationalParser extends AdditiveGroupParser[Rational] {
    protected val signlessInteger: P[Rational] = safeLongParser.signlessParser.map(Rational(_))
    protected val signlessFraction: P[Rational] = (safeLongParser.signlessParser ~ "/" ~ safeLongParser.signlessPositive).map {
      case (n, d) => Rational(n, d)
    }
    val signlessParser: P[Rational] = P( signlessFraction | signlessInteger ).opaque("<nonneg-rational>")
    val nonNegative: P[Rational] = P("+".? ~ signlessParser)
    val negative: P[Rational] = ("-" ~ signlessParser).map(r => -r)
    val parser: P[Rational] = P(negative | nonNegative).opaque("<rational>")
  }

  val `unaryop_-` = Prefix("-", 10)
  val `op_^` = Infix.rightAssoc("*", 20)
  val `invop_*` = Infix.leftAssoc("", 30)
  val `op_*` = Infix.leftAssoc("*", 30)
  val `op_/` = Infix.leftAssoc("/", 30)
  val `op_+` = Infix.leftAssoc("+", 40)
  val `op_-` = Infix.leftAssoc("-", 40)
  val `op_,` = Infix.leftAssoc(",", 100) // lowest priority

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
