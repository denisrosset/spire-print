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

  def pretty[A](a: A)(implicit pp: PrettyPrint[A]): String = {
    implicit val sb = PrettyStringBuilder.newBuilder()
    pp.print(a)
    sb.toString
  }

  def apply[A](implicit ev: PrettyPrint[A]): PrettyPrint[A] = ev

}
