package spire.print

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSuite, Matchers}
import org.typelevel.discipline.scalatest.Discipline

trait SpirePrintSuite extends FunSuite with Matchers
  with PropertyChecks
  with spire.syntax.AllSyntax
