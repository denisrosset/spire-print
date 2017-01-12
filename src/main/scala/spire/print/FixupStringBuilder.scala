package spire.print

import spire.syntax.cfor._
import java.lang.{StringBuilder => JavaStringBuilder}

import scala.annotation.tailrec

/** A variant of java.lang.StringBuilder/scala.collection.mutable.StringBuilder optimized for performance.
  * An underlying [[java.lang.StringBuilder]] is used to append strings. Strings can also be inserted in the middle
  * of the [[FixupStringBuilder]] in a lazy manner: the position and the content of those "fixups" is registered and
  * those are inserted
  */
class FixupStringBuilder {

  private[this] val underlying: JavaStringBuilder = new JavaStringBuilder
  private[this] var numFix: Int = 0
  private[this] var fixPos: Array[Int] = null
  private[this] var fixString: Array[String] = null

  /** Gets the length of the underlying [[java.lang.StringBuilder]]. This length is not affected by the
    * registered fixups. */
  def length: Int = underlying.length

  def append(b: Boolean): this.type = { underlying.append(b); this }

  def append(c: Char): this.type = { underlying.append(c); this }

  def append(str: Array[Char]): this.type = { underlying.append(str); this }

  def append(str: Array[Char], offset: Int, len: Int): this.type = { underlying.append(str, offset, len); this }

  def append(s: CharSequence): this.type = { underlying.append(s); this }

  def append(s: CharSequence, start: Int, end: Int): this.type = { underlying.append(s, start, end); this }

  def append(d: Double): this.type = { underlying.append(d); this }

  def append(f: Float): this.type = { underlying.append(f); this }

  def append(i: Int): this.type = { underlying.append(i); this }

  def append(l: Long): this.type = { underlying.append(l); this }

  def append(obj: AnyRef): this.type = { underlying.append(obj); this }

  def append(str: String): this.type = { underlying.append(str); this }

  def append(sb: StringBuffer): this.type = { underlying.append(sb); this }

  def appendCodePoint(codePoint: Int): this.type = { underlying.append(codePoint); this }

  /** Registers the insertion of `string` at the position `pos`. The underlying string builder is not modified:
    * its length, the position of previously inserted characters is not changed. These insertions will be performed
    * when calling `toString`.
    */
  def addFixup(pos: Int, string: String): this.type = {
    if ((fixPos eq null) || numFix == fixPos.length) {
      val newLength = scala.math.max(4, numFix * 2)
      val newFixPos = new Array[Int](newLength)
      val newFixString = new Array[String](newLength)
      if (numFix != 0) {
        Array.copy(fixPos, 0, newFixPos, 0, numFix)
        Array.copy(fixString, 0, newFixString, 0, numFix)
      }
      fixPos = newFixPos
      fixString = newFixString
    }
    fixPos(numFix) = pos
    fixString(numFix) = string
    numFix += 1
    this
  }

  override def toString: String = if (numFix == 0) underlying.toString else {
    ParQuickSort.qsort(fixPos, fixString, 0, numFix - 1)(spire.std.int.IntAlgebra, implicitly)
    @tailrec def finalLength(acc: Int, i: Int): Int = if (i == numFix) acc else finalLength(acc + fixString(i).length, i + 1)
    val sb = new JavaStringBuilder(finalLength(underlying.length, 0))
    @tailrec def append(beforeFix: Int): Unit = {
      val start = if (beforeFix == 0) 0 else fixPos(numFix - 1)
      val end = if (beforeFix == numFix) underlying.length else fixPos(numFix)
      sb.append(underlying, start, end - start)
      if (beforeFix < numFix) {
        sb.append(fixString(beforeFix))
        append(beforeFix + 1)
      }
    }
    append(0)
    sb.toString
  }

}

object FixupStringBuilder {

  def newBuilder(): FixupStringBuilder = new FixupStringBuilder

}