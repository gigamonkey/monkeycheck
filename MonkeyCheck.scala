package org.scalacheck

import scala.util.Random

object MonkeyCheck {

  import Between._

  // Properties are what we check automatically.
  type Property[-T] = T => Boolean

  case class Parameters(size: Int, random: Random)

  // Generators generate arbitrary inputs to properties.
  type Generator[+T] = Parameters => Option[T]

  trait G[T] extends (Parameters => Option[T]) {

    def apply(p: Parameters): Option[T]

    def shrink(value: T): Stream[T] = Stream.empty

    def map[U](fn: T => U): G[U] = new G[U] {
      def apply(p: Parameters) = G.this.apply(p).map(fn)
    }

    def flatMap[U](fn: T => G[U]): G[U] = new G[U] {
      def apply(p: Parameters) = G.this.apply(p).flatMap(fn(_).apply(p))
    }

    def filter(fn: T => Boolean): G[T] = new G[T] {
      def apply(p: Parameters) = G.this.apply(p).filter(fn)
      override def shrink(value: T) = G.this.shrink(value).filter(fn)
    }

  }

  implicit class RichGenerator[T](g: Generator[T]) {
    def map[U](fn: T => U): Generator[U]                = { ps => g(ps).map(fn) }
    def flatMap[U](fn: T => Generator[U]): Generator[U] = { ps => g(ps).flatMap(fn(_)(ps)) }
    def filter(fn: T => Boolean): Generator[T]          = { ps => g(ps).filter(fn) }
  }


  // Public methods for getting generators.

  def arbitrary[T](ps: Parameters)(implicit g: Generator[T]): Option[T] = g.apply(ps)

  def generator[T](implicit g: Generator[T]): Generator[T] = g
  def between[T](min: T, max: T)(implicit b: Between[T]): Generator[T] = b(min, max)
  def oneOf[T](xs: Seq[T]): Generator[T] = between(0, xs.size - 1).map(xs(_))
  def oneOf[T](t: T, ts: T*): Generator[T] = oneOf[T](t +: ts)

  def betwixt[T](min: T, max: T, p: Parameters)(implicit b: Between[T]): Option[T] = b(min, max)(p)

  // Arbitrary generators are the base case generators for when all we
  // know about what we want is the type. If you want to have them
  // accessible, you'll want to import MonkeyCheck.Arbitrary._
  object Arbitrary {

    implicit lazy val byteG = new G[Byte] {
      def apply(p: Parameters) = betwixt(Byte.MinValue, Byte.MaxValue, p)
    }

    implicit lazy val arbitraryByte:    Generator[Byte]    = between(Byte.MinValue, Byte.MaxValue)
    implicit lazy val arbitraryChar:    Generator[Char]    = between(Char.MinValue, Char.MaxValue)
    implicit lazy val arbitraryShort:   Generator[Short]   = between(Short.MinValue, Short.MaxValue)
    implicit lazy val arbitraryInt:     Generator[Int]     = between(Int.MinValue, Int.MaxValue)
    implicit lazy val arbitraryLong:    Generator[Long]    = between(Long.MinValue, Long.MaxValue)
    implicit lazy val arbitraryFloat:   Generator[Float]   = between(Float.MinValue, Float.MaxValue)
    implicit lazy val arbitraryDouble:  Generator[Double]  = between(Double.MinValue, Double.MaxValue)
    implicit lazy val arbitraryBoolean: Generator[Boolean] = { ps => Some(ps.random.nextBoolean) }
    implicit lazy val arbitraryUnit:    Generator[Unit]    = { ps => Some(()) }

    implicit lazy val arbitraryAnyVal: Generator[AnyVal] = { ps =>
      oneOf(
        arbitraryByte, arbitraryChar, arbitraryShort, arbitraryInt, arbitraryLong,
        arbitraryFloat, arbitraryDouble, arbitraryBoolean, arbitraryUnit)
        .apply(ps)
        .flatMap(_.apply(ps))
    }

    // In scalacheck, the arbitrary option goes through some
    // machinations with the size. At least part of the point, I
    // think, is that we probably want to mostly return Somes whereas
    // this returns None half the time.
    implicit def arbitraryOption[T:Generator]: Generator[Option[T]] = { ps =>
      if (ps.random.nextBoolean) None else Some(generator[T].apply(ps)) }

    implicit def arbitraryEither[T:Generator, U:Generator]: Generator[Either[T, U]] = { ps =>
      if (ps.random.nextBoolean) {
        arbitrary[T](ps).map(Left(_))
      } else {
        arbitrary[U](ps).map(Right(_))
      }
    }

    ////////////////////////////////////////////////////////////////////////
    // Tuples

    implicit def arbitraryTuple1[T:Generator] =
      for { t <- generator[T] } yield Tuple1(t)

    implicit def arbitraryTuple2[T1:Generator, T2:Generator] =
      for {
        t1 <- generator[T1]
        t2 <- generator[T2]
      } yield (t1, t2)

    implicit def arbitraryTuple3[T1:Generator, T2:Generator, T3:Generator] =
      for {
        t1 <- generator[T1]
        t2 <- generator[T2]
        t3 <- generator[T3]
      } yield (t1, t2, t3)

    // In some sense the previous generator knows how it generated the
    // three parts of the tuple. To make a stream of shrunk tuples
    // from a particular tuple it needs to make one stream for each
    // element with values that contain a shrunk version of that
    // element and the other two as in the original tuple being
    // shrunk. Is this generally applicable? I.e. can the output of a
    // generator that has been created by flatMapping always be



    // TODO: tuples up to 22

    // TODO: Buildable containers, both seqs and maps.


    // Not clear how useful these arbitrary functions really are since
    // each generated function returns the same value regardless of
    // input. Perhaps of more use would be something that given a set
    // of properties of a function, can generate a function that
    // satisfies those properties. Thus if you have some code you
    // actually need to test that takes a function as an argument

    implicit def arbitraryFunction1[T1,R:Generator]: Generator[T1 => R] = { ps =>
      for { r <- arbitrary[R](ps) } yield (t1: T1) => r
    }

    implicit def arbitraryFunction2[T1,T2,R:Generator]: Generator[(T1, T2) => R] = { ps =>
      for { r <- arbitrary[R](ps) } yield (t1: T1, t2: T2) => r
    }

  }
  object Between {
    // Used internally to make generators for ordered things like numbers.
    type Between[T] = (T, T) => Generator[T]

    private def chooseLong(low: Long, high: Long, random: => Long): Option[Long] = {
      if (low <= high) Some(low + math.abs(random % ((high - low) + 1))) else None
    }

    private def chooseDouble(low: Double, high: Double, random: => Double): Option[Double] = {
      if (low <= high) Some(low + math.abs(random % ((high - low) + 1))) else None
    }

    implicit val betweenByte:   Between[Byte]   = (low: Byte, high: Byte)     => { ps => chooseLong(low, high, ps.random.nextLong).map(_.toByte) }
    implicit val betweenChar:   Between[Char]   = (low: Char, high: Char)     => { ps => chooseLong(low, high, ps.random.nextLong).map(_.toChar) }
    implicit val betweenShort:  Between[Short]  = (low: Short, high: Short)   => { ps => chooseLong(low, high, ps.random.nextLong).map(_.toShort) }
    implicit val betweenInt:    Between[Int]    = (low: Int, high: Int)       => { ps => chooseLong(low, high, ps.random.nextLong).map(_.toInt) }
    implicit val betweenLong:   Between[Long]   = (low: Long, high: Long)     => { ps => chooseLong(low, high, ps.random.nextLong) }
    implicit val betweenFloat:  Between[Float]  = (low: Float, high: Float)   => { ps => chooseDouble(low, high, ps.random.nextDouble).map(_.toInt) }
    implicit val betweenDouble: Between[Double] = (low: Double, high: Double) => { ps => chooseDouble(low, high, ps.random.nextDouble).map(_.toInt) }

  }

}
