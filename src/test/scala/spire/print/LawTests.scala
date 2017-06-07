package spire.print

import spire.std.any._
import spire.laws.arb.{safeLong => sla, rational => ra}
import defaults._
import spire.math.{Rational, SafeLong}

class LawTests extends SpirePrintSuite {

  checkAll("PrintParseLaws[Int].printParse", PrintParseLaws[Int].printParse)
  checkAll("PrintParseLaws[SafeLong].printParse", PrintParseLaws[SafeLong].printParse)
  checkAll("PrintParseLaws[Rational].printParse", PrintParseLaws[Rational].printParse)

}
