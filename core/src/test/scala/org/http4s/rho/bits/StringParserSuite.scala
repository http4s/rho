package org.http4s.rho.bits

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.{Date, UUID}

import cats.effect.IO
import munit.FunSuite
import org.http4s.rho.bits.StringParserSuite.UserId

class StringParserSuite extends FunSuite {
  test("A StringParser should parse true") {
    assertEquals(new BooleanParser[IO]().parse("true"), SuccessResponse[IO, Boolean](true))
  }

  test("A StringParser should parse false") {
    assertEquals(new BooleanParser[IO]().parse("false"), SuccessResponse[IO, Boolean](false))
  }

  test("A StringParser should not parse non-boolean") {
    new BooleanParser[IO]().parse("unknown") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse double") {
    assertEquals(new DoubleParser[IO]().parse("1.1"), SuccessResponse[IO, Double](1.1))
  }

  test("A StringParser should parse non-double") {
    new DoubleParser[IO]().parse("unknown") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse float") {
    assertEquals(new FloatParser[IO]().parse("1.1"), SuccessResponse[IO, Float](1.1f))
  }

  test("A StringParser should parse non-float") {
    new FloatParser[IO]().parse("unknown") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse int") {
    assertEquals(new IntParser[IO]().parse("1"), SuccessResponse[IO, Int](1))
  }

  test("A StringParser should parse non-int") {
    new IntParser[IO]().parse("12345678901") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse long") {
    assertEquals(new LongParser[IO]().parse("1"), SuccessResponse[IO, Long](1))
  }

  test("A StringParser should parse non-long") {
    new LongParser[IO]().parse("12345678901234567890") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse short") {
    assertEquals(new ShortParser[IO]().parse("1"), SuccessResponse[IO, Short](1))
  }

  test("A StringParser should parse non-short") {
    new ShortParser[IO]().parse("12345678") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse date") {
    assertEquals(
      new DateParser[IO]().parse("2018-02-09"),
      SuccessResponse[IO, Date](
        new SimpleDateFormat("yyyy-MM-dd").parse("2018-02-09")
      )
    )
  }

  test("A StringParser should parse non-date") {
    new DateParser[IO]().parse("2018-09") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse instant") {
    assertEquals(
      new InstantParser[IO]().parse("2018-02-09T00:00:00Z"),
      SuccessResponse[IO, Instant](
        Instant.parse("2018-02-09T00:00:00Z")
      )
    )
  }

  test("A StringParser should parse non-instant") {
    new InstantParser[IO]().parse("2018-02-09 00:00:00Z") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A StringParser should parse uuid") {
    assertEquals(
      new UUIDParser[IO]()
        .parse("459043db-c29e-4dd9-a36d-b3a11b5eeb17"),
      SuccessResponse[IO, UUID](
        UUID.fromString("459043db-c29e-4dd9-a36d-b3a11b5eeb17")
      )
    )
  }

  test("A StringParser should parse non-uuid") {
    new UUIDParser[IO]()
      .parse("459043b-4dd9-a36d-b3a11b5eeb17") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  val mappedParser = new LongParser[IO].map(UserId)

  test("A mapped StringParser should succeed when base parser has succeeded") {
    assertEquals(mappedParser.parse("123"), SuccessResponse[IO, UserId](UserId(123L)))
  }

  test("A mapped StringParser should fail when base parser has failed") {
    mappedParser.parse("abc") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  val rmappedParser = new IntParser[IO].rmap(i =>
    if (i >= 0) SuccessResponse(i)
    else FailureResponseOps[IO].badRequest("Only non-negative integers are accepted.")
  )

  test(
    "A RMapped StringParser should succeed when base parser has succeeded and the mapping function has returned a SuccessResponse"
  ) {
    assertEquals(rmappedParser.parse("1"), SuccessResponse[IO, Int](1))
  }

  test("A RMapped StringParser should fail when base parser has failed") {
    rmappedParser.parse("abc") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }

  test("A RMapped StringParser should fail when mapping function has returned a FailureResponse") {
    rmappedParser.parse("-1") match {
      case _: FailureResponse[IO] => ()
      case _ => fail("Unexpected parse result found")
    }
  }
}

object StringParserSuite {
  case class UserId(id: Long)
}
