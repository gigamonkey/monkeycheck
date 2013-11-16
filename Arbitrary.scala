package com.gigamonkeys.monkeycheck

import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Random

// Arbitrary generators are simply some handy generators for common
// types organized for easy importation.
object Arbitrary {

  import Generator._

  object Numbers {
    import Between._

    object + {
      implicit lazy val positiveByte:   Generator[Byte]    = between(1, Byte.MaxValue)
      implicit lazy val positiveShort:  Generator[Short]   = between(1, Short.MaxValue)
      implicit lazy val positiveInt:    Generator[Int]     = between(1, Int.MaxValue)
      implicit lazy val positiveLong:   Generator[Long]    = between(1, Long.MaxValue)
      implicit lazy val positiveFloat:  Generator[Float]   = between(Float.MinPositiveValue, Float.MaxValue)
      implicit lazy val positiveDouble: Generator[Double]  = between(Double.MinPositiveValue, Double.MaxValue)
    }

    object - {
      implicit lazy val negativeByte:   Generator[Byte]    = between(Byte.MinValue, -1)
      implicit lazy val negativeShort:  Generator[Short]   = between(Short.MinValue, -1)
      implicit lazy val negativeInt:    Generator[Int]     = between(Int.MinValue, -1)
      implicit lazy val negativeLong:   Generator[Long]    = between(Long.MinValue, -1)
      implicit lazy val negativeFloat:  Generator[Float]   = between(Float.MinValue, -1 * Float.MinPositiveValue)
      implicit lazy val negativeDouble: Generator[Double]  = between(Double.MinValue, -1 * Double.MinPositiveValue)
    }

    object !- {
      implicit lazy val nonNegativeByte:   Generator[Byte]    = between(0, Byte.MaxValue)
      implicit lazy val nonNegativeShort:  Generator[Short]   = between(0, Short.MaxValue)
      implicit lazy val nonNegativeInt:    Generator[Int]     = between(0, Int.MaxValue)
      implicit lazy val nonNegativeLong:   Generator[Long]    = between(0, Long.MaxValue)
      implicit lazy val nonNegativeFloat:  Generator[Float]   = between(0, Float.MaxValue)
      implicit lazy val nonNegativeDouble: Generator[Double]  = between(0, Double.MaxValue)
    }

    implicit lazy val arbitraryByte:    Generator[Byte]    = between(Byte.MinValue, Byte.MaxValue)
    implicit lazy val arbitraryShort:   Generator[Short]   = between(Short.MinValue, Short.MaxValue)
    implicit lazy val arbitraryInt:     Generator[Int]     = between(Int.MinValue, Int.MaxValue)
    implicit lazy val arbitraryLong:    Generator[Long]    = between(Long.MinValue, Long.MaxValue)
    implicit lazy val arbitraryFloat:   Generator[Float]   = between(Float.MinValue, Float.MaxValue)
    implicit lazy val arbitraryDouble:  Generator[Double]  = between(Double.MinValue, Double.MaxValue)

  }

  implicit lazy val arbitraryBoolean: Generator[Boolean] = oneOf(true, false)
  implicit lazy val arbitraryUnit:    Generator[Unit]    = const(())

  implicit lazy val arbitraryAnyVal:  Generator[AnyVal] = new Generator[AnyVal] {
    def apply(p: Parameters) =
      oneOf(
        Numbers.arbitraryByte,
        Numbers.arbitraryShort,
        Numbers.arbitraryInt,
        Numbers.arbitraryLong,
        Numbers.arbitraryFloat,
        Numbers.arbitraryDouble,
        Characters.Unicode.unicodeChar,
        arbitraryBoolean,
        arbitraryUnit).apply(p).flatMap(_.apply(p))
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

  object Tuples {
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

    implicit def arbitraryTuple3[T1:Generator, T2:Generator, T3:Generator] =
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

    implicit def arbitraryTuple5[T1:Generator, T2:Generator, T3:Generator, T4:Generator, T5:Generator] =
      (for {
        t1 <- generator[T1]
        t2 <- generator[T2]
        t3 <- generator[T3]
        t4 <- generator[T4]
        t5 <- generator[T5]
      } yield (t1, t2, t3, t4, t5)).shrinkWith {
        case (t1, t2, t3, t4, t5) =>
          generator[T1].shrink(t1).map((_, t2, t3, t4, t5)) append
          generator[T2].shrink(t2).map((t1, _, t3, t4, t5)) append
          generator[T3].shrink(t3).map((t1, t2, _, t4, t5)) append
          generator[T4].shrink(t4).map((t1, t2, t3, _, t5)) append
          generator[T5].shrink(t5).map((t1, t2, t3, t4, _))
      }

    // TODO: tuples up to 22
  }

  object Collections {

    implicit def collection[T:Generator, C[_]](implicit c: CanBuildFrom[Nothing, T, C[T]]): Generator[C[T]] = new Generator[C[T]] {
      def apply(p: Parameters) = {
        val b = c.apply
          (1 to p.size).foreach { _ =>
            arbitrary[T](p).foreach { t => b += t }
          }
        Some(b.result)
      }
    }

    import Tuples.arbitraryTuple2

    implicit def collection2[T:Generator, U:Generator, C[_,_]](implicit c: CanBuildFrom[Nothing, (T, U), C[T, U]]): Generator[C[T, U]] = new Generator[C[T, U]] {
      def apply(p: Parameters) = {
        val b = c.apply
          (1 to p.size).foreach { _ =>
            arbitrary[(T,U)](p).foreach { t => b += t }
          }
        Some(b.result)
      }
    }

  }

  object Characters {
    import Between._

    object Unicode {
      implicit lazy val unicodeChar: Generator[Char] = between(Char.MinValue, Char.MaxValue)
    }

    object Ascii {
      implicit lazy val asciiChar: Generator[Char] = between(0, 256)
    }

    object Alphanumeric {
      implicit lazy val asciiChar: Generator[Char] = oneOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789".toList)
    }
  }

  object Strings {
    import Collections._

    object Unicode {
      import Characters.Unicode._
      implicit val unicodeString: Generator[String] = generator[Seq[Char]].map(_.mkString)
    }

    object Ascii {
      import Characters.Ascii._
      implicit val asciiString: Generator[String] = generator[Seq[Char]].map(_.mkString)
    }

    object Alphanumeric {
      import Characters.Alphanumeric._
      implicit val alphanumericString: Generator[String] = generator[Seq[Char]].map(_.mkString)
    }
  }

  object FunctionCalls {

    import Tuples._

    def resultOf[T:Generator, R](f: T => R) = generator[T].map(f)

    def resultOf[
      T1:Generator,
      T2:Generator,
      R
    ](f: (T1, T2) => R) = generator[(T1, T2)].map(f.tupled)

    def resultOf[
      T1:Generator,
      T2:Generator,
      T3:Generator,
      R
    ](f: (T1, T2, T3) => R) = generator[(T1, T2, T3)].map(f.tupled)

    def resultOf[
      T1:Generator,
      T2:Generator,
      T3:Generator,
      T4:Generator,
      R
    ](f: (T1, T2, T3, T4) => R) = generator[(T1, T2, T3, T4)].map(f.tupled)

    def resultOf[
      T1:Generator,
      T2:Generator,
      T3:Generator,
      T4:Generator,
      T5:Generator,
      R
    ](f: (T1, T2, T3, T4, T5) => R) = generator[(T1, T2, T3, T4, T5)].map(f.tupled)

    // TODO: resultOf for functions up to 22 parameters
  }


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

object Between {

  // Used internally to make generators for ordered things like numbers.
  type Between[T] = (T, T) => Generator[T] // FIXME T should really be orderable.

  // This is still not quite right since while a double can
  // represent the magnitude of the difference between Long.MinValue
  // and Long.MaxValue it loses precision. We handle that specific
  // case specially but similarly large ranges will have the same
  // problem, I think.
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
}
