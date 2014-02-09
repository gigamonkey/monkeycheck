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

  def check[T](property: Property[T], params: Parameters): Result[T] = property.check(params)

  def check2[T](property: Property[T], params: Parameters): Unit = {
    println((property.label.getOrElse("unlabled") + " ").padTo(60, '.') + " " +
      (property.check(params) match {
        case Passed() => "okay."
        case Failed(t) => "whoops! Failed at: " + t.getOrElse("unknown")
      }))
  }

  def main(args: Array[String]) {
    import Property._

    val params = Parameters(10, new Random, 10)

    def betweenProperties() {
      import Generator.between
      import Between._
      import Arbitrary.Numbers._
      check2(
        forAll((i1: Int, i2: Int) => {
          val g = between(i1, i2)
          g(params).map { v =>
            i1 <= v && v <= i2
          }.getOrElse(true)
        }).labeled("between int in range"), Parameters(10, new Random, 100000))
      }


    def strings() {
      import Arbitrary.Strings.Unicode._
      check2(forAll { (s1: String, s2: String) => (s1 + s2).length == s1.length + s2.length }.labeled("string length"), params)
      check2(forAll { (s1: String) => s1.reverse.reverse == s1 }.labeled("string reverse"), params)
      check2(forAll { (s: String) => s.toUpperCase.toLowerCase == s.toLowerCase }.labeled("string upper/lower"), params)
    }

    def alphanumericStrings() {
      import Arbitrary.Strings.Alphanumeric._
      check2(forAll { (s: String) => println(s); true }.labeled("alphanumeric strings"), params)
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
    betweenProperties()
  }
}
