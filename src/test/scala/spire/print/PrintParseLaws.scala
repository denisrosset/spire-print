package spire.print

import org.scalacheck.{Arbitrary, Prop}
import org.typelevel.discipline.Laws
import spire.algebra.Eq
import org.scalacheck.Prop._
import spire.syntax.eq._

object PrintParseLaws {

  def apply[A:Eq:Arbitrary] = new PrintParseLaws[A] {
    def equ = Eq[A]
    def arb = implicitly[Arbitrary[A]]
  }

}

trait PrintParseLaws[A] extends Laws {

  implicit def equ: Eq[A]
  implicit def arb: Arbitrary[A]

  def printParse(implicit A: Parser[A], P: PrettyPrint[A]) = new OperationProperties(
    name = "printParse",
    parent = None,
    bases = Seq.empty,
    "roundtrip" -> forAll { (a: A) =>
      A.parser.parse(PrettyPrint.pretty(a)).get.value === a
    }
  )

  class OperationProperties(
                             val name: String,
                             val parent: Option[OperationProperties],
                             val bases: Seq[(String, Laws#RuleSet)],
                             val props: (String, Prop)*
                           ) extends RuleSet with HasOneParent

}
