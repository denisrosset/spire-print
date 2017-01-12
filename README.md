Fast pretty-printing with operator precedence handling
======================================================

This is an implementation of the algorithm described in:

> N. Ramsey, “Unparsing expressions with prefix and postfix operators,”
> Software: Practice and Experience, vol. 28, no. 12, pp. 1327–1356, 1998.

See the [original paper](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.39.1314&rep=rep1&type=pdf).

A similar implementation is available in the library [Kiama](https://bitbucket.org/inkytonik/kiama/src/4c429aa2dfda2f34f731d12fb9878859b7c4be66/library/src/main/scala/org/bitbucket/inkytonik/kiama/output/ParenPrettyPrinter.scala?at=default&fileviewer=file-view-default). The major difference is that our implementation uses mutable state, does not allocate objects for intermediate values, and should have speed comparable to `scala.collection.mutable.StringBuilder`.

Algorithm outline
-----------------

We proceed as follows.

When an object `a: A` is pretty-printed, the compiler looks for a `PrettyPrint[A]` implicit, that "knows" how to provide a string representation of objects of type `A`. Instead of building a `String` from smaller pieces and paying a heavy allocation tax, a `PrettyStringBuilder` is allocated and passed to the `print` method of `PrettyPrint[A]`, along with the value to print.

The `print` method will append a text representation of `a` in the allocated `PrettyStringBuilder`, and return the outermost operator in the expression.

This return value is important when the content that was just printed is part of a bigger expresion; here, as we just called `print` with the whole expression, this return value is discarded.

However, when `print` is called recursively for the subparts of the expression, this return value is used to check whether the subpart that has been just printed needs to be enclosed by parentheses. Due to the mutable nature of our algorithm, it is then "too late" to add an opening `(` at the right place; instead, the insertion of `(` is registered inside the `PrettyStringBuilder`. After the expression has finished printing, the `PrettyStringBuilder.toString` method is called and the required `(` are inserted when building the returned `String`.

The "abstract syntax tree" of the printed expression is implicitly constructed by `print` calling the instances of binary or unary operators that build the subexpressions. For example, here are the common operators used in arithmetic expresions:

```scala

  val `unaryop_-` = Prefix(symbol = "-", priority = 10) // priority: lower couples more tightly
  val `invop_*` = Infix.leftAssoc("", 30)
  val `op_*` = Infix.leftAssoc("*", 30)
  val `op_/` = Infix.leftAssoc("/", 30)
  val `op_+` = Infix.leftAssoc("+", 40)
  val `op_-` = Infix.leftAssoc("-", 40)

```

Here is then a simple routine to pretty-print rational numbers:

```scala

  implicit object safeLong extends PrettyPrint[SafeLong] {

    def print(a: SafeLong)(implicit s: PrettyStringBuilder): Res = {
      if (a.isValidLong)
        Atom(a.toLong) // Atom has the side-effect of printing the value into the implicit PrettyStringBuffer
      else
        Atom(a.toString)
      if (a.signum < 0) `unaryop_-` else Atom // correct after the fact that the outermost operator is -, if that is the case
    }

  }

  implicit object rational extends PrettyPrint[Rational] {

    def print(a: Rational)(implicit s: PrettyStringBuilder): Res = {
      if (a.isWhole) {
        if (a.isValidLong) Atom(a.numeratorAsLong) else Atom(a.toString)
        if (a.signum < 0) `unaryop_-` else Atom
      } else `op_/`(a.numerator, a.denominator) // use the / operator on two subparts
    }

  }

```
