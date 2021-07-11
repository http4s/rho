package org.http4s.rho
package bits

import cats.effect.IO
import munit.FunSuite
import org.http4s.rho.io._

class ResultResponseSuite extends FunSuite {
  test("A ResultResponse should map a SuccessResponse") {
    assertEquals(SuccessResponse(3).map(_.toString), SuccessResponse("3"))
  }

  test("A ResultResponse should map a FailureResponse") {
    val result: ResultResponse[IO, String] = badRequest("foo")
    assertEquals(result.map(_.toString), result)
  }

  test("A ResultResponse should flatMap a SuccessResponse") {
    assertEquals(
      SuccessResponse[IO, Int](3).flatMap(i => SuccessResponse(i.toString)),
      SuccessResponse[IO, String]("3")
    )
  }

  test("A ResultResponse should flatMap a FailureResponse") {
    val result: ResultResponse[IO, String] = badRequest("foo")
    assertEquals(result.flatMap(i => SuccessResponse(i.toString)), result)
  }

  test("A ResultResponse should work in a for comprehensions") {
    val a = SuccessResponse("foo")
    val b = SuccessResponse("bar")

    val result = for {
      foo <- a
      bar <- b
    } yield foo + bar

    assertEquals(result, SuccessResponse("foobar"))
  }

  test("A ResultResponse should work in a failing for comprehensions") {
    val a: ResultResponse[IO, String] = badRequest("boo")
    val b: ResultResponse[IO, String] = SuccessResponse("bar")

    val result1 = for {
      foo <- a
      bar <- b
    } yield foo + bar

    val result2 = for {
      bar <- b
      foo <- a
    } yield foo + bar

    assertEquals(result1, a)
    assertEquals(result2, a)
  }
}
