package quantum

import spire.math.Complex
import spire.implicits._
import scala.util.{Try, Success, Failure}

// BitString operations using Long as bitmask
case class BitString(bits: Long, length: Int) {
  require(length >= 0 && length <= 64, "BitString length must be between 0 and 64")

  // Get the bit at position i (0 = rightmost bit)
  def apply(i: Int): Int =
    if ((bits >> i) & 1L) == 1L then 1 else 0

  // Set bit at position i to value (0 or 1)
  def updated(i: Int, value: Int): BitString = {
    require(value == 0 || value == 1, "Bit value must be 0 or 1")
    if (value == 1) {
      BitString(bits | (1L << i), length)
    } else {
      BitString(bits & ~(1L << i), length)
    }
  }

  // Flip bit at position i
  def flip(i: Int): BitString =
    BitString(bits ^ (1L << i), length)

  override def toString: String = {
    (0 until length).map(i => apply(length - 1 - i)).mkString
  }
}

object BitString {
  def zero(length: Int): BitString = BitString(0L, length)

  def fromString(s: String): BitString = {
    val bits = s.reverse.zipWithIndex.foldLeft(0L) { case (acc, (char, i)) =>
      if (char == '1') acc | (1L << i) else acc
    }
    BitString(bits, s.length)
  }
}


