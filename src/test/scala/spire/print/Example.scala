package spire.print

import spire.print.Op.{Infix, Prefix}

sealed trait AST

object AST {
  import defaults._

  case class Cons(i: Int) extends AST
  case class Sum(list: List[AST]) extends AST
  case class Prod(list: List[AST]) extends AST
  case class Inv(a: AST) extends AST
  case class Neg(a: AST) extends AST

  implicit object parenPrint extends PrettyPrint[AST] {

    val NegOp = Prefix("-", 20)
    val Add = Infix.leftAssoc("+", 40)
    val Sub = Infix.leftAssoc("-", 40)
    val Mul = Infix.leftAssoc("*", 30)
    val Div = Infix.leftAssoc("/", 30)

    def print(a: AST)(implicit sb: PrettyStringBuilder): Res = a match {
      case Cons(i) => Atom(i.toString)
      case Inv(a) => Div(Cons(1), a)
      case Neg(a) => NegOp(a)
      case Sum(list) => list.reverse match {
        case last :: Nil => print(last)
        case Neg(last) :: prev => Sub(Sum(prev.reverse), last)
        case last :: prev => Add(Sum(prev.reverse), last)
        case Nil => Atom("0")
      }
      case Prod(list) => list.reverse match {
        case last :: Nil => print(last)
        case Inv(last) :: prev => Div(Prod(prev.reverse), last)
        case last :: prev => Mul(Prod(prev.reverse), last)
        case Nil => Atom("1")
      }
    }

  }

}

class ASTSuite extends SpirePrintSuite {

  import AST.{Prod, Neg, Sum, Cons}

  test("AST pretty-printing") {
    val ast = Prod(List(Cons(3), Neg(Cons(2)), Cons(3), Sum(List(Cons(3), Neg(Cons(2))))))
    PrettyPrint.pretty(ast) shouldBe "3*-2*3*(3-2)"
  }

}

