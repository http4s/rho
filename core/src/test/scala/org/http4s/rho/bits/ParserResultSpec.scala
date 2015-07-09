//package org.http4s.rho
//package bits
//
//
//import org.http4s.rho.bits.ResponseGeneratorInstances.BadRequest
//import org.specs2.mutable.Specification
//
//
//class ParserResultSpec extends Specification {
//
//  "ParserResult" should {
//
//    "map a Success" in {
//      ParserSuccess(3).map(_.toString) should_== ParserSuccess("3")
//    }
//
//    "map a ValidationFailure" in {
//      val result: ParserResult[Int] = ValidationFailure(BadRequest("foo"))
//      result.map(_.toString) should_== result
//    }
//
//    "map a ParseFailure" in {
//      val result: ParserResult[Int] = ParserFailure("foo")
//      result.map(_.toString) should_== result
//    }
//
//    "flatMap a Success" in {
//      ParserSuccess(3).flatMap(i => ParserSuccess(i.toString)) should_== ParserSuccess("3")
//    }
//
//    "flatMap a ValidationFailure" in {
//      val result: ParserResult[Int] = ValidationFailure(BadRequest("foo"))
//      result.flatMap(i => ParserSuccess(i.toString)) should_== result
//    }
//
//    "flatMap a ParseFailure" in {
//      val result: ParserResult[Int] = ParserFailure("foo")
//      result.flatMap(i => ParserSuccess(i.toString)) should_== result
//    }
//
//    "work in a for comprehensions" in {
//      val a = ParserSuccess("foo")
//      val b = ParserSuccess("bar")
//      val result = for {
//        foo <- a
//        bar <- b
//      } yield (foo + bar)
//
//      result should_== ParserSuccess("foobar")
//    }
//
//    "work in a failing for comprehensions" in {
//      val a: ParserResult[String] = ParserFailure("boo")
//      val b: ParserResult[String] = ParserSuccess("bar")
//      val result1 = for {
//        foo <- a
//        bar <- b
//      } yield (foo + bar)
//
//      val result2 = for {
//        bar <- b
//        foo <- a
//      } yield (foo + bar)
//
//      (result1 should_== ParserFailure("boo")) and
//      (result2 should_== ParserFailure("boo"))
//    }
//  }
//
//}
