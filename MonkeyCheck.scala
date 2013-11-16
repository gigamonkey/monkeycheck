package com.gigamonkeys.monkeycheck

import scala.annotation.tailrec
import scala.collection.mutable.Builder
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util.Random

object MonkeyCheck {

  def show[T](label: String, r: Result[T]) = {
    println((label + " ").padTo(60, '.') + " " +
      (r match {
        case Passed() => "okay."
        case Failed(t) => "whoops! Failed at: " + t.getOrElse("unknown")
      }))
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
    loop(10, 0, 0)
  }

  def main(args: Array[String]) {
    import Property._

    val params = Parameters(10, new Random)

    def strings() {
      import Arbitrary.Strings.Unicode._
      show("string length",      check(forAll { (s1: String, s2: String) => println(s1 + s2); (s1 + s2).length == s1.length + s2.length }, params))
      show("string reverse",     check(forAll { (s1: String) => println(s1); s1.reverse.reverse == s1 }, params))
      show("string upper/lower", check(forAll { (s: String) => println(s); s.toUpperCase.toLowerCase == s.toLowerCase }, params))
    }

    def alphanumericStrings() {
      import Arbitrary.Strings.Alphanumeric._
      show("alphanumeric strings", check(forAll { (s: String) => println(s); true }, params))
    }

    def allNumbers() {
      import Arbitrary.Numbers._
      val p = forAll { (i: Int) => i + 2 > i }
      show("i + 2 > i",          check(p, params))
      show("i * 10 > i",         check((i: Long) => i * 10 > i, params))
      show("* commutative",      check(forAll((i1: Int, i2: Int) => i1 * i2 == i2 * i1), params))
      show("exists i * 10 < i",  check(exists((i: Int) => i * 10 < i), params))
      show("exists i * 10 == i", check(exists((i: Short) => i * 10 == i), params))
      show("i * 10 != i",        check((i: Short) => i * 10 != i, params))
    }

    def positiveNumbers() {
      import Arbitrary.Numbers.+._
      show("sums greater than their parts", check(forAll((i1: Int, i2: Int) => i1 + i2 > i1 && i1 + i2 > i2), params))
    }

    def collections() {
      import Arbitrary.Numbers._
      import Arbitrary.Tuples._
      import Arbitrary.Collections._
      import Arbitrary.Strings.Ascii._

      show("tuples", check(forAll { (s: (Int, String, Double)) => println(s); true }, params))
      show("set of ints", check(forAll { (s: Set[Int]) => println(s); true }, params))
      show("list of tuples", check(forAll { (s: List[(Int, Set[Int])]) => println(s); true }, params))
      show("maps", check(forAll { (m: Map[Int, Byte]) => println(m); true }, params))
    }

    strings()
    alphanumericStrings()
    allNumbers()
    positiveNumbers()
    collections()
  }
}
