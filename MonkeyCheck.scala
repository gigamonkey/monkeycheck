package org.scalacheck

import scala.util.Random

object MonkeyCheck {

  import Between._

  // Properties are what we check automatically.
  type Property[-T] = T => Boolean

  case class Parameters(size: Int, random: Random)

  trait Generator[T] extends (Parameters => Option[T]) {

    def apply(p: Parameters): Option[T]

    def shrink(value: T): Stream[T] = Stream.empty

    def map[U](fn: T => U): Generator[U] = new Generator[U] {
      def apply(p: Parameters) = Generator.this(p).map(fn)
    }

    def flatMap[U](fn: T => Generator[U]): Generator[U] = new Generator[U] {
      def apply(p: Parameters) = Generator.this(p).flatMap(fn(_)(p))
    }

    def filter(fn: T => Boolean): Generator[T] = new Generator[T] {
      def apply(p: Parameters) = Generator.this.apply(p).filter(fn)
      override def shrink(value: T) = Generator.this.shrink(value).filter(fn)
    }

    def shrinkWith(fn: T => Stream[T]) = new Generator[T] {
      def apply(p: Parameters) = Generator.this.apply(p)
      override def shrink(value: T) = fn(value)
    }
  }

  // Public methods for getting generators.

  def arbitrary[T](ps: Parameters)(implicit g: Generator[T]): Option[T] = g.apply(ps)
  def generator[T](implicit g: Generator[T]): Generator[T] = g
  def between[T](min: T, max: T)(implicit b: Between[T]): Generator[T] = b(min, max)
  def oneOf[T](xs: Seq[T]): Generator[T] = between(0, xs.size - 1).map(xs(_))
  def oneOf[T](t: T, ts: T*): Generator[T] = oneOf[T](t +: ts)
  def const[T](t: T) = new Generator[T] { def apply(p: Parameters) = Some(t) }

  // Arbitrary generators are the base case generators for when all we
  // know about what we want is the type. If you want to have them
  // accessible, you'll want to import MonkeyCheck.Arbitrary._
  object Arbitrary {

    implicit lazy val arbitraryByte:    Generator[Byte]    = between(Byte.MinValue, Byte.MaxValue)
    implicit lazy val arbitraryChar:    Generator[Char]    = between(Char.MinValue, Char.MaxValue)
    implicit lazy val arbitraryShort:   Generator[Short]   = between(Short.MinValue, Short.MaxValue)
    implicit lazy val arbitraryInt:     Generator[Int]     = between(Int.MinValue, Int.MaxValue)
    implicit lazy val arbitraryLong:    Generator[Long]    = between(Long.MinValue, Long.MaxValue)
    implicit lazy val arbitraryFloat:   Generator[Float]   = between(Float.MinValue, Float.MaxValue)
    implicit lazy val arbitraryDouble:  Generator[Double]  = between(Double.MinValue, Double.MaxValue)
    implicit lazy val arbitraryBoolean: Generator[Boolean] = oneOf(true, false)
    implicit lazy val arbitraryUnit:    Generator[Unit]    = const(())

    implicit lazy val arbitraryAnyVal:  Generator[AnyVal] = new Generator[AnyVal] {
      def apply(p: Parameters) =
        oneOf(
          arbitraryByte, arbitraryChar, arbitraryShort, arbitraryInt, arbitraryLong,
          arbitraryFloat, arbitraryDouble, arbitraryBoolean, arbitraryUnit)
          .apply(p).flatMap(_.apply(p))
    }

    // In scalacheck, the arbitrary option goes through some
    // machinations with the size. At least part of the point, I
    // think, is that we probably want to mostly return Somes whereas
    // this returns None half the time.
    implicit def arbitraryOption[T:Generator]: Generator[Option[T]] = new Generator[Option[T]] {
      def apply(p: Parameters) = if (p.random.nextBoolean) None else Some(generator[T].apply(p))
    }

    implicit def arbitraryEither[T:Generator, U:Generator]: Generator[Either[T, U]] = new Generator[Either[T, U]] {
      def apply(p: Parameters) =
        if (p.random.nextBoolean) {
          arbitrary[T](p).map(Left(_))
        } else {
          arbitrary[U](p).map(Right(_))
        }
    }

    ////////////////////////////////////////////////////////////////////////
    // Tuples

    implicit def arbitraryTuple1[T1:Generator] =
      (for {
        t1 <- generator[T1]
      } yield Tuple1(t1)).shrinkWith {
        case Tuple1(t1) =>
          generator[T1].shrink(t1).map(Tuple1(_))
      }

    implicit def arbitraryTuple2[T1:Generator, T2:Generator] =
      (for {
        t1 <- generator[T1]
        t2 <- generator[T2]
      } yield (t1, t2)).shrinkWith {
        case (t1, t2) =>
          generator[T1].shrink(t1).map((_, t2)) append
          generator[T2].shrink(t2).map((t1, _))
      }

    implicit def arbitraryTuple2[T1:Generator, T2:Generator, T3:Generator] =
      (for {
        t1 <- generator[T1]
        t2 <- generator[T2]
        t3 <- generator[T3]
      } yield (t1, t2, t3)).shrinkWith {
        case (t1, t2, t3) =>
          generator[T1].shrink(t1).map((_, t2, t3)) append
          generator[T2].shrink(t2).map((t1, _, t3)) append
          generator[T3].shrink(t3).map((t1, t2, _))
      }

    implicit def arbitraryTuple4[T1:Generator, T2:Generator, T3:Generator, T4:Generator] =
      (for {
        t1 <- generator[T1]
        t2 <- generator[T2]
        t3 <- generator[T3]
        t4 <- generator[T4]
      } yield (t1, t2, t3, t4)).shrinkWith {
        case (t1, t2, t3, t4) =>
          generator[T1].shrink(t1).map((_, t2, t3, t4)) append
          generator[T2].shrink(t2).map((t1, _, t3, t4)) append
          generator[T3].shrink(t3).map((t1, t2, _, t4)) append
          generator[T4].shrink(t4).map((t1, t2, t3, _))
      }

    // TODO: tuples up to 22

    // TODO: Buildable containers, both seqs and maps.


    // Not clear how useful these arbitrary functions really are since
    // each generated function returns the same value regardless of
    // input. Perhaps of more use would be something that given a set
    // of properties of a function, can generate a function that
    // satisfies those properties. Thus if you have some code you
    // actually need to test that takes a function as an argument

    implicit def arbitraryFunction1[T1,R:Generator]: Generator[T1 => R] = new Generator[T1 => R] {
      def apply(p: Parameters) = for { r <- arbitrary[R](p) } yield (t1: T1) => r
    }

    implicit def arbitraryFunction2[T1,T2,R:Generator]: Generator[(T1, T2) => R] = new Generator[(T1, T2) => R] {
      def apply(p: Parameters) = for { r <- arbitrary[R](p) } yield (t1: T1, t2: T2) => r
    }
  }

  object NonNegativeNumbers {

    implicit lazy val nonNegativeByte:   Generator[Byte]    = between(0, Byte.MaxValue)
    implicit lazy val nonNegativeShort:  Generator[Short]   = between(0, Short.MaxValue)
    implicit lazy val nonNegativeInt:    Generator[Int]     = between(0, Int.MaxValue)
    implicit lazy val nonNegativeLong:   Generator[Long]    = between(0, Long.MaxValue)
    implicit lazy val nonNegativeFloat:  Generator[Float]   = between(0, Float.MaxValue)
    implicit lazy val nonNegativeDouble: Generator[Double]  = between(0, Double.MaxValue)

  }

  object NegativeNumbers {

    implicit lazy val negativeByte:   Generator[Byte]    = between(Byte.MinValue, -1)
    implicit lazy val negativeShort:  Generator[Short]   = between(Short.MinValue, -1)
    implicit lazy val negativeInt:    Generator[Int]     = between(Int.MinValue, -1)
    implicit lazy val negativeLong:   Generator[Long]    = between(Long.MinValue, -1)
    implicit lazy val negativeFloat:  Generator[Float]   = between(Float.MinValue, -1)
    implicit lazy val negativeDouble: Generator[Double]  = between(Double.MinValue, -1)

  }

  object Between {

    // Used internally to make generators for ordered things like numbers.
    type Between[T] = (T, T) => Generator[T]

    // This is still not quite right since while a double can
    // represent the magnitude of the difference between Long.MinValue
    // and Long.MaxValue it looses precision. We handle that specific
    // case specially but similarly large ranges will have the same
    // problem, I think.
    private def choose(low: Double, high: Double, random: => Double) = if (low <= high) Some(low + (((high - low) + 1) * random)) else None

    implicit val betweenByte: Between[Byte] = (low: Byte, high: Byte) => new Generator[Byte] {
      def apply(ps: Parameters) = choose(low, high, ps.random.nextDouble).map(_.toByte)
    }
    implicit val betweenChar: Between[Char] = (low: Char, high: Char) => new Generator[Char] {
      def apply(ps: Parameters) = choose(low, high, ps.random.nextDouble).map(_.toChar)
    }
    implicit val betweenShort: Between[Short] = (low: Short, high: Short) => new Generator[Short] {
      def apply(ps: Parameters) = choose(low, high, ps.random.nextDouble).map(_.toShort)
    }
    implicit val betweenInt: Between[Int] = (low: Int, high: Int) => new Generator[Int] {
      def apply(ps: Parameters) = choose(low, high, ps.random.nextDouble).map(_.toInt)
    }
    implicit val betweenLong: Between[Long] = (low: Long, high: Long) => new Generator[Long] {
      def apply(ps: Parameters) =
        // See comment above at choose.
        if (low == Long.MinValue && high == Long.MaxValue) {
          Some(ps.random.nextLong)
        } else {
          choose(low, high, ps.random.nextDouble).map(_.toLong)
        }
    }
    implicit val betweenFloat: Between[Float] = (low: Float, high: Float) => new Generator[Float] {
      def apply(ps: Parameters) = choose(low, high, ps.random.nextDouble).map(_.toFloat)
    }
    implicit val betweenDouble: Between[Double] = (low: Double, high: Double) => new Generator[Double] {
      def apply(ps: Parameters) = choose(low, high, ps.random.nextDouble)
    }
  }
}
