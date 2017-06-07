package spire.print

import spire.syntax.cfor._
import java.lang.{StringBuilder => JavaStringBuilder}

import scala.annotation.tailrec
import scala.reflect.ClassTag

import spire.algebra.Order

/** A variant of java.lang.StringBuilder/scala.collection.mutable.StringBuilder optimized for performance.
  * An underlying [[java.lang.StringBuilder]] is used to append strings. Strings can also be inserted in the middle
  * of the [[PrettyStringBuilder]] in a lazy manner: the position and the content of those "fixups" is registered and
  * those are inserted
  */
class PrettyStringBuilder {

  import PrettyStringBuilder.Pos

  private[this] val underlying: JavaStringBuilder = new JavaStringBuilder
  private[this] var numFix: Int = 0
  private[this] var fixPos: Array[Int] = null
  private[this] var fixString: Array[String] = null

  def currentPos: Pos = Pos(underlying.length)

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

  def appendCodePoint(codePoint: Int): this.type = { underlying.appendCodePoint(codePoint); this }

  /** Registers the insertion of `string` at the position `pos`. The underlying string builder is not modified:
    * its length, the position of previously inserted characters is not changed. These insertions will be performed
    * when calling `toString`.
    */
  def addFixup(pos: Pos, string: String): this.type = {
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
    fixPos(numFix) = pos.i
    fixString(numFix) = string
    numFix += 1
    this
  }

  /** Sorts the position and string arrays as the same entity. We use insertion sort as the array is likely
    * to have a small number of elements (< 100) and to be partly sorted. */
  private[this] def sortFix(start: Int = 0, end: Int = numFix - 1): Unit = {
    var i = start + 1
    while (i < end) {
      val item = fixPos(i)
      val itemB = fixString(i)
      var hole = i
      while (hole > start && fixPos(hole - 1) > item) {
        fixPos(hole) = fixPos(hole - 1)
        fixString(hole) = fixString(hole - 1)
        hole -= 1
      }
      fixPos(hole) = item
      fixString(hole) = itemB
      i += 1
    }
  }

  override def toString: String = if (numFix == 0) underlying.toString else {
    @tailrec def finalLength(acc: Int, i: Int): Int = if (i == numFix) acc else finalLength(acc + fixString(i).length, i + 1)
    val dest = new JavaStringBuilder(finalLength(underlying.length, 0))
    printInto(dest)
    dest.toString
  }

  def printInto(dest: JavaStringBuilder): Unit = {
    sortFix()
    @tailrec def iterate(beforeFix: Int): Unit = {
      val start = if (beforeFix == 0) 0 else fixPos(beforeFix - 1)
      val end = if (beforeFix == numFix) underlying.length else fixPos(beforeFix)
      dest.append(underlying, start, end)
      if (beforeFix < numFix) {
        dest.append(fixString(beforeFix))
        iterate(beforeFix + 1)
      }
    }
    iterate(0)
  }

}

object PrettyStringBuilder {

  def newBuilder(): PrettyStringBuilder = new PrettyStringBuilder

  case class Pos(val i: Int) extends AnyVal

}
