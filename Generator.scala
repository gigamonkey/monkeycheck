package com.gigamonkeys.monkeycheck

import scala.util.Random

case class Parameters(
  size: Int,
  random: Random,
  iterations: Int
)

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
  import Between._

  /*
   * Get the implicitly available generator for the given type.
   */
  def generator[T](implicit g: Generator[T]): Generator[T] = g

  /*
   * Make a generator of T that returns one of the elements of the given sequence.
   */
  def oneOf[T](xs: Seq[T]): Generator[T] = between(0, xs.size - 1).map(xs(_))

  /*
   * Make a generator of T that returns one of the argument values.
   */
  def oneOf[T](t: T, ts: T*): Generator[T] = oneOf(t +: ts)

  /*
   * Make a generator that always returns the same value.
   */
  def const[T](t: T) = new Generator[T] { def apply(p: Parameters) = Some(t) }

  /*
   * Make a generator that returns a T between a given min and max, inclusive.
   */
  def between[T](min: T, max: T)(implicit b: Between[T]): Generator[T] = new Generator[T] {
    def apply(ps: Parameters) = b(min, max, ps.random)
  }

  /*
   * Get an arbitrary T using the implicitly available generator of T.
   */
  def arbitrary[T](ps: Parameters)(implicit g: Generator[T]): Option[T] = g(ps)

}
