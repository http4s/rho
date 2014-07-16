package org.http4s.rho.bits

import org.specs2.mutable.Specification


class ParserResultSpec extends Specification {

  "ParserResult" should {

    "map a Success" in {
      Success(3).map(_.toString) should_== Success("3")
    }

    "map a ValidationFailure" in {
      val result: ParserResult[Int] = ValidationFailure("foo")
      result.map(_.toString) should_== result
    }

    "map a ParseFailure" in {
      val result: ParserResult[Int] = ParseFailure("foo")
      result.map(_.toString) should_== result
    }

    "flatMap a Success" in {
      Success(3).flatMap(i => Success(i.toString)) should_== Success("3")
    }

    "flatMap a ValidationFailure" in {
      val result: ParserResult[Int] = ValidationFailure("foo")
      result.flatMap(i => Success(i.toString)) should_== result
    }

    "flatMap a ParseFailure" in {
      val result: ParserResult[Int] = ParseFailure("foo")
      result.flatMap(i => Success(i.toString)) should_== result
    }

    "work in a for comprehensions" in {
      val a = Success("foo")
      val b = Success("bar")
      val result = for {
        foo <- a
        bar <- b
      } yield (foo + bar)

      result should_== Success("foobar")
    }

    "work in a failing for comprehensions" in {
      val a: ParserResult[String] = ParseFailure("boo")
      val b: ParserResult[String] = Success("bar")
      val result1 = for {
        foo <- a
        bar <- b
      } yield (foo + bar)

      val result2 = for {
        bar <- b
        foo <- a
      } yield (foo + bar)

      (result1 should_== ParseFailure("boo")) and
      (result2 should_== ParseFailure("boo"))
    }
  }

}
