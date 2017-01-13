package spire.print

import spire.algebra.{IsReal, Semiring, Signed}
import spire.math.{Complex, Polynomial}

/** A typeclass that returns whether the expression representing a value starts with a negative term, i.e. a
  * term that begins with the `unary_-` operator.
  *
  * Examples of such terms are:
  *
  * - negative values such as SafeLong(-2) or `Rational(-3,4)`
  * - polynomials whose leading coefficient starts with a `unary_-`,
  * - complex numbers whose first nonzero real/imag part is negative.
  *
  * This typeclass is used in polynomial printing to replace the binary `+` operator by `-`, and printing the opposite
  * coefficient.
  */
trait NegativeFirstTerm[-A] {

  def apply(a: A): Boolean

}

abstract class DefaultNegativeFirstTerm {

  object default extends NegativeFirstTerm[Any] {
    def apply(a: Any): Boolean = false
  }

  implicit def fromAny[A]: NegativeFirstTerm[A] = default

}

object NegativeFirstTerm {

  def apply[A](implicit ev: NegativeFirstTerm[A]): NegativeFirstTerm[A] = ev

  implicit def fromSigned[A](implicit s: Signed[A]): NegativeFirstTerm[A] = new NegativeFirstTerm[A] {
    def apply(a: A) = s.isSignNegative(a)
  }

  implicit def fromComplex[A](implicit ir: IsReal[A]): NegativeFirstTerm[Complex[A]] = new NegativeFirstTerm[Complex[A]] {
    def apply(c: Complex[A]) =
      if (c.isImaginary) ir.isSignNegative(c.imag) else ir.isSignNegative(c.real)
  }

  implicit def fromPolynomial[A:Semiring](implicit s: NegativeFirstTerm[A]): NegativeFirstTerm[Polynomial[A]] = new NegativeFirstTerm[Polynomial[A]] {
    def apply(p: Polynomial[A]): Boolean =
      if (p.isConstant) s(p.nth(0)) else s(p.termsIterator.next().coeff)
  }

}
