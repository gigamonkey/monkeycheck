package com.gigamonkeys.monkeycheck

import scala.annotation.tailrec
import scala.util.Random

// Used internally to make generators for ordered things like numbers.
object Between {

  type Between[T] = (T, T) => Generator[T] // FIXME T should really be orderable.

  def between[T](min: T, max: T)(implicit b: Between[T]): Generator[T] = b(min, max)

  implicit val betweenByte: Between[Byte] = (low: Byte, high: Byte) => new Generator[Byte] {
    def apply(ps: Parameters) = choose(low, high, ps.random).map(_.toByte)
  }
  implicit val betweenChar: Between[Char] = (low: Char, high: Char) => new Generator[Char] {
    def apply(ps: Parameters) = choose(low, high, ps.random).map(_.toChar)
  }
  implicit val betweenShort: Between[Short] = (low: Short, high: Short) => new Generator[Short] {
    def apply(ps: Parameters) = choose(low, high, ps.random).map(_.toShort)
  }
  implicit val betweenInt: Between[Int] = (low: Int, high: Int) => new Generator[Int] {
    def apply(ps: Parameters) = choose(low, high, ps.random).map(_.toInt)
  }
  implicit val betweenLong: Between[Long] = (low: Long, high: Long) => new Generator[Long] {
    def apply(ps: Parameters) = choose(low, high, ps.random).map(_.toLong)
  }
  implicit val betweenFloat: Between[Float] = (low: Float, high: Float) => new Generator[Float] {
    def apply(ps: Parameters) = choose(low, high, ps.random).map(_.toFloat)
  }
  implicit val betweenDouble: Between[Double] = (low: Double, high: Double) => new Generator[Double] {
    def apply(ps: Parameters) = choose(low, high, ps.random)
  }

  // I'm not 100% sure this is right but it seems reasonably close to
  // correct. A Double can represent the difference (and the
  // mid-point) between even the most extreme Double values as long as
  // we take some care about how we compute the
  // difference, i.e. (high/2 - low/2) rather than (high - low)/2.
  private def choose(min: Double, max: Double, random: Random) = {
    @tailrec def loop(low: Double, high: Double): Double = {
      if (low == high) {
        low
      } else {
        val mid = low + (high/2 - low/2)
        if (random.nextBoolean) loop(low, mid) else loop(mid, high)
      }
    }
    if (min <= max) Some(loop(min, max)) else None
  }
}
