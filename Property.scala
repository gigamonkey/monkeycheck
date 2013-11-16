package com.gigamonkeys.monkeycheck

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Random

// Not clear right level to do this as we may want to combine
// Properties rather than Predicates.
/*implicit class RichPredicate[T](p: Predicate[T]) {

  def &&(other: Predicate[T]) = (t: T) => p(t) && other(t)

  def ||(other: Predicate[T]) = (t: T) => p(t) || other(t)

  def infix_!() = (t: T) => !p(t)

}
 */
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
) extends (Parameters => Option[Evidence[T]]) {

  def check(params: Parameters): Result[T] = {
    @tailrec def loop(iters: Int, support: Int, trials: Int): Result[T] = {
      if (iters == 0) {
        if (trials > 0 && (support.toDouble / trials) > .8) Passed() else Failed(None)
      } else {
        apply(params) match {
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
    loop(10, 0, 0)
  }


}

object Property {

  import Arbitrary.Tuples._

  // Default conversion of a predicate to a property is to capture
  // the appropriate generator and assert that the predicate should
  // hold for all generated values.
  implicit def toProperty[T](p: Predicate[T])(implicit g: Generator[T]) = new Property(p, g) {
    def apply(params: Parameters) = generator(params).map { t =>
      if (predicate(t)) Support(t) else Falsified(t)
    }
  }

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

  // TODO: forAll for functions up to 22 parameters

  // Less frequently used in tests, ocassionally it's useful to be
  // able to assert that a predicate holds for at least one
  // generated value.
  def exists[T](p: Predicate[T])(implicit g: Generator[T]) = new Property(p, g) {
    def apply(params: Parameters) = generator(params).map { t =>
      if (predicate(t)) Proved(t) else Undecided(t)
    }
  }
}
