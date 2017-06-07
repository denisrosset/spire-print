package spire.print

import fastparse.WhitespaceApi
import fastparse.noApi.P

trait Parser[+A] {
  def parser: P[A]
}

trait AdditiveGroupParser[+A] extends Parser[A] {
  def signlessParser: P[A]
}

object Parser {

  def apply[A](p: P[A]): Parser[A] =
    new Parser[A] {
      def parser = p
    }

}
