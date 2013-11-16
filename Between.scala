package com.gigamonkeys.monkeycheck

import scala.annotation.tailrec
import scala.util.Random

// Used internally to make generators for ordered things like numbers.
object Between {

  type Between[T] = (T, T, Random) => Option[T] // FIXME T should really be orderable.

  implicit val betweenByte:   Between[Byte]   = (low, high, random) => choose(low, high, random).map(_.toByte)
  implicit val betweenChar:   Between[Char]   = (low, high, random) => choose(low, high, random).map(_.toChar)
  implicit val betweenShort:  Between[Short]  = (low, high, random) => choose(low, high, random).map(_.toShort)
  implicit val betweenInt:    Between[Int]    = (low, high, random) => choose(low, high, random).map(_.toInt)
  implicit val betweenLong:   Between[Long]   = (low, high, random) => choose(low, high, random).map(_.toLong)
  implicit val betweenFloat:  Between[Float]  = (low, high, random) => choose(low, high, random).map(_.toFloat)
  implicit val betweenDouble: Between[Double] = (low, high, random) => choose(low, high, random)

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
