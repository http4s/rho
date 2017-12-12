package org.http4s.rho
package bits


import cats.effect.IO
import org.specs2.mutable.Specification


class ResultResponseSpec extends Specification {

  "ResultResponse" should {

    "map a SuccessResponse" in {
      SuccessResponse(3).map(_.toString) should_== SuccessResponse("3")
    }

    "map a FailureResponse" in {
      val result: ResultResponse[IO, Int] = FailureResponse.badRequest("foo")
      result.map(_.toString) should_== result
    }

    "flatMap a SuccessResponse" in {
      SuccessResponse[IO, Int](3).flatMap(i => SuccessResponse(i.toString)) should_== SuccessResponse[IO, String]("3")
    }

    "flatMap a FailureResponse" in {
      val result: ResultResponse[IO, Int] = FailureResponse.badRequest("foo")
      result.flatMap(i => SuccessResponse(i.toString)) should_== result
    }

    "work in a for comprehensions" in {
      val a = SuccessResponse("foo")
      val b = SuccessResponse("bar")

      val result = for {
        foo <- a
        bar <- b
      } yield foo + bar

      result should_== SuccessResponse("foobar")
    }

    "work in a failing for comprehensions" in {
      val a: ResultResponse[IO, String] = FailureResponse.badRequest("boo")
      val b: ResultResponse[IO, String] = SuccessResponse("bar")

      val result1 = for {
        foo <- a
        bar <- b
      } yield foo + bar

      val result2 = for {
        bar <- b
        foo <- a
      } yield foo + bar

      (result1 should_== a) and (result2 should_== a)
    }
  }

}
