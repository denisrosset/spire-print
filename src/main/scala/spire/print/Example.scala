package spire.print

import spire.util.Opt

object FourOps {

  import Op._

  val NegOp = Prefix("-", 20)
  val Add = Infix.ofLeft("+", 40)
  val Sub = Infix.ofLeft("-", 40)
  val Mul = Infix.ofLeft("*", 30)
  val Div = Infix.ofLeft("/", 40)

}

sealed trait AST

object AST {

  implicit object parenPrint extends ParenPrint[AST] {

    import ParenPrint.{atom, prefix, postfix, infix}
    import FourOps._

    def print(a: AST) = a match {
      case Cons(i) => atom(i.toString)
      case Inv(a) => infix(Cons(1), Div, a)
      case Neg(a) => prefix(NegOp, a)
      case Sum(list) => list.reverse match {
        case last :: Nil => print(last)
        case Neg(last) :: prev => infix(Sum(prev.reverse), Sub, last)
        case last :: prev => infix(Sum(prev.reverse), Add, last)
        case Nil => atom("0")
      }
      case Prod(list) => list.reverse match {
        case last :: Nil => print(last)
        case Inv(last) :: prev => infix(Prod(prev.reverse), Div, last)
        case last :: prev => infix(Prod(prev.reverse), Mul, last)
        case Nil => atom("1")
      }
    }

  }

}

case class Cons(i: Int) extends AST
case class Sum(list: List[AST]) extends AST
case class Prod(list: List[AST]) extends AST
case class Inv(a: AST) extends AST
case class Neg(a: AST) extends AST

object Test extends App {

  val ast = Prod(List(Cons(3), Neg(Cons(2)), Cons(3), Sum(List(Cons(3), Neg(Cons(2))))))

  println(ParenPrint(ast))

}
