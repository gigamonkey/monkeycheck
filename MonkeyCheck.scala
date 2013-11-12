package com.gigamonkeys.monkeycheck

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.util.Random

object MonkeyCheck {

  import Between._
  import Generator._

  trait Generator[T] extends (Parameters => Option[T]) { outer =>

    def apply(p: Parameters): Option[T]

    def shrink(value: T): Stream[T] = Stream.empty

    def map[U](fn: T => U): Generator[U] = new Generator[U] {
      def apply(p: Parameters) = outer(p).map(fn)
    }

    def flatMap[U](fn: T => Generator[U]): Generator[U] = new Generator[U] {
      def apply(p: Parameters) = outer(p).flatMap(fn(_)(p))
    }

    def filter(fn: T => Boolean): Generator[T] = new Generator[T] {
      def apply(p: Parameters) = outer(p).filter(fn)
      override def shrink(value: T) = outer.shrink(value).filter(fn)
    }

    def shrinkWith(fn: T => Stream[T]): Generator[T] = new Generator[T] {
      def apply(p: Parameters) = outer(p)
      override def shrink(value: T) = fn(value)
    }
  }

  object Generator {
    def generator[T](implicit g: Generator[T]): Generator[T]              = g
    def between[T](min: T, max: T)(implicit b: Between[T]): Generator[T]  = b(min, max)
    def oneOf[T](xs: Seq[T]): Generator[T]                                = between(0, xs.size - 1).map(xs(_))
    def oneOf[T](t: T, ts: T*): Generator[T]                              = oneOf(t +: ts)
    def const[T](t: T)                                                    = new Generator[T] { def apply(p: Parameters) = Some(t) }
    def arbitrary[T](ps: Parameters)(implicit g: Generator[T]): Option[T] = g(ps)
  }

  object Between {

    // Used internally to make generators for ordered things like numbers.
    type Between[T] = (T, T) => Generator[T]

    // This is still not quite right since while a double can
    // represent the magnitude of the difference between Long.MinValue
    // and Long.MaxValue it looses precision. We handle that specific
    // case specially but similarly large ranges will have the same
    // problem, I think.
    private def choose(low: Double, high: Double, random: => Double) =
      if (low <= high) Some(low + (((high - low) + 1) * random)) else None

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

  // Arbitrary generators are simply some handy generators for common
  // types organized for easy importation.
  object Arbitrary {

    object Numbers {

      object + {
        implicit lazy val positiveByte:   Generator[Byte]    = between(1, Byte.MaxValue)
        implicit lazy val positiveShort:  Generator[Short]   = between(1, Short.MaxValue)
        implicit lazy val positiveInt:    Generator[Int]     = between(1, Int.MaxValue)
        implicit lazy val positiveLong:   Generator[Long]    = between(1, Long.MaxValue)
        implicit lazy val positiveFloat:  Generator[Float]   = between(1, Float.MaxValue)
        implicit lazy val positiveDouble: Generator[Double]  = between(1, Double.MaxValue)
      }

      object - {
        implicit lazy val negativeByte:   Generator[Byte]    = between(Byte.MinValue, -1)
        implicit lazy val negativeShort:  Generator[Short]   = between(Short.MinValue, -1)
        implicit lazy val negativeInt:    Generator[Int]     = between(Int.MinValue, -1)
        implicit lazy val negativeLong:   Generator[Long]    = between(Long.MinValue, -1)
        implicit lazy val negativeFloat:  Generator[Float]   = between(Float.MinValue, -1)
        implicit lazy val negativeDouble: Generator[Double]  = between(Double.MinValue, -1)
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

    implicit lazy val arbitraryChar:    Generator[Char]    = between(Char.MinValue, Char.MaxValue)
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
          arbitraryChar,
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

    }
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

  type Predicate[-T] = T => Boolean

  // Not clear right level to do this as we may want to combine
  // Properties rather than Predicates.
  implicit class RichPredicate[T](p: Predicate[T]) {

    def &&(other: Predicate[T]) = (t: T) => p(t) && other(t)

    def ||(other: Predicate[T]) = (t: T) => p(t) || other(t)

    def infix_!() = (t: T) => !p(t)

  }

  case class Parameters(size: Int, random: Random)

  // Each time we check a Property we get a piece of evidence
  // concerning the unerlying predicate. By checking the same property
  // many times with different inputs we can amass enough evidence to
  // return a Result. Undecided evidence only arises in exists
  // properties because a single check that fails only tells us that
  // that particular input didn't satisfy the property.
  sealed trait Evidence[T]

  // Based on value, the property was proved to hold. (E.g. an exists
  // property can be proven with a single value.)
  case class Proved[T](value: T) extends Evidence[T]

  // Based on the value, the property was proved not to hold. (E.g. a
  // forAll property can be falsified by a single counter example.)
  case class Falsified[T](value: T) extends Evidence[T]

  // The value tested provides support that the property could be
  // true. (E.g. a specific value that passes a forAll property
  // check.)
  case class Support[T](value: T) extends Evidence[T]

  // The value provided neither supports our belief in the property
  // nor disproves it. (E.g. a value that fails an exists property.)
  case class Undecided[T](value: T) extends Evidence[T]

  ////////////////////////////////////////////////////////////////////////
  // Checking a Property many times yields a result, either be being
  // actualy Proved or Falsified or, after we have done all our
  // checks, based on the ratio of support to checks.
  sealed trait Result[T]
  case class Passed[T]() extends Result[T]
  case class Failed[T](value: Option[T]) extends Result[T]

  // A Property is a function that can be passed a set of parameters
  // for a generator and tries to return a Evidence. To actually check
  // a Property we need to call it multiple times and make sure it
  // never fails.
  abstract class Property[T](
    val predicate: Predicate[T],
    val generator: Generator[T]
  ) extends (Parameters => Option[Evidence[T]])

  object Property {

    import Arbitrary.Tuples._

    implicit def toProperty[T](p: Predicate[T])(implicit g: Generator[T]) = new Property(p, g) {
      def apply(params: Parameters) = generator(params).map { t =>
        if (predicate(t)) Support(t) else Falsified(t)
      }
    }

    // Default conversion of a predicate to a property is to capture
    // the appropriate generator and assert that the predicat should
    // hold for all generated values.
    def forAll[T:Generator](p: Predicate[T]) = toProperty(p)

    def forAll[
      T1:Generator,
      T2:Generator]
      (fn: (T1, T2) => Boolean) = toProperty(fn.tupled)

    def forAll[
      T1:Generator,
      T2:Generator,
      T3:Generator]
      (fn: (T1, T2, T3) => Boolean) = toProperty(fn.tupled)

    def forAll[
      T1:Generator,
      T2:Generator,
      T3:Generator,
      T4:Generator]
      (fn: (T1, T2, T3, T4) => Boolean) = toProperty(fn.tupled)

    def forAll[
      T1:Generator,
      T2:Generator,
      T3:Generator,
      T4:Generator,
      T5:Generator]
      (fn: (T1, T2, T3, T4, T5) => Boolean) = toProperty(fn.tupled)

    // Less frequently used in tests, ocassionally it's useful to be
    // able to assert that a predicate holds for at least one
    // generated value.
    def exists[T](p: Predicate[T])(implicit g: Generator[T]) = new Property(p, g) {
      def apply(params: Parameters) = generator(params).map { t =>
        if (predicate(t)) Proved(t) else Undecided(t)
      }
    }
  }

  def check[T](property: Property[T], params: Parameters): Result[T] = {
    @tailrec def loop(iters: Int, support: Int, trials: Int): Result[T] = {
      if (iters == 0) {
        if (trials > 0 && (support.toDouble / trials) > .8) Passed() else Failed(None)
      } else {
        property(params) match {
          case Some(Proved(_))    => Passed()
          case Some(Falsified(t)) => Failed(Some(t))
          case Some(Support(_))   => loop(iters - 1, support + 1, trials + 1)
          case Some(Undecided(_)) => loop(iters - 1, support, trials + 1)
          case None               => loop(iters - 1, support, trials)
        }
      }
    }

    // FIXME: the number of iterations should be attached to the
    // propety itself, perhaps derived from the generator if not
    // otherwise specified.
    loop(1000000, 0, 0)
  }


}

object HelloWorld {

  def main(args: Array[String]) {
    import MonkeyCheck._
    import MonkeyCheck.Property._

    def show[T](label: String, r: Result[T]) = {
      println((label + " ").padTo(60, '.') + " " +
        (r match {
          case Passed() => "okay."
          case Failed(t) => "whoops! Failed at: " + t.getOrElse("unknown")
        }))
    }

    val params = Parameters(10, new Random)

    implicit val sgen: Generator[String] = new Generator[String] {
      def apply(params: Parameters) = {
        import MonkeyCheck.Arbitrary._
        import MonkeyCheck.Generator.arbitrary
        val sb = new StringBuilder(params.size)
        for (i <- 1 to params.size) { arbitrary[Char](params).foreach { c => sb + c } }
        Some(sb.toString)
      }
    }

    show("string length", check(forAll((s1: String, s2: String) => (s1 + s2).length == s1.length + s2.length), params))
    show("string reverse", check(forAll((s1: String) => s1.reverse.reverse == s1), params))
    show("string upper/lower", check(forAll((s: String) => s.toUpperCase.toLowerCase == s.toLowerCase), params))

    def allNumbers() {
      import MonkeyCheck.Arbitrary.Numbers._
      val p = forAll { (i: Int) => i + 2 > i }
      show("i + 2 > i",          check(p, params))
      show("i * 10 > i",         check((i: Long) => i * 10 > i, params))
      show("* commutative",      check(forAll((i1: Int, i2: Int) => i1 * i2 == i2 * i1), params))
      show("exists i * 10 < i",  check(exists((i: Int) => i * 10 < i), params))
      show("exists i * 10 == i", check(exists((i: Short) => i * 10 == i), params))
      show("i * 10 != i",        check((i: Short) => i * 10 != i, params))
    }

    def positiveNumbers() {
      import MonkeyCheck.Arbitrary.Numbers.+._
      show("sums greater than their parts", check(forAll((i1: Int, i2: Int) => i1 + i2 > i1 && i1 + i2 > i2), params))
    }

    allNumbers()
    positiveNumbers()

  }
}
