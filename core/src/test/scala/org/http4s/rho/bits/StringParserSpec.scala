package org.http4s.rho.bits

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.{Date, UUID}

import cats.effect.IO
import org.http4s.rho.bits.StringParserSpec.UserId
import org.specs2.mutable.Specification

class StringParserSpec extends Specification {

  "StringParser" should {

    "parse true" in {
      new BooleanParser[IO]().parse("true") === SuccessResponse(true)
    }
    "parse false" in {
      new BooleanParser[IO]().parse("false") === SuccessResponse(false)
    }
    "not parse non-boolean" in {
      new BooleanParser[IO]().parse("unknown") must haveClass[FailureResponse[IO]]
    }

    "parse double" in {
      new DoubleParser[IO]().parse("1.1") === SuccessResponse[IO, Double](1.1)
    }
    "parse non-double" in {
      new DoubleParser[IO]().parse("unknown") must haveClass[FailureResponse[IO]]
    }

    "parse float" in {
      new FloatParser[IO]().parse("1.1") === SuccessResponse[IO, Float](1.1f)
    }
    "parse non-float" in {
      new FloatParser[IO]().parse("unknown") must haveClass[FailureResponse[IO]]
    }

    "parse int" in {
      new IntParser[IO]().parse("1") === SuccessResponse[IO, Int](1)
    }
    "parse non-int" in {
      new IntParser[IO]().parse("12345678901") must haveClass[FailureResponse[IO]]
    }

    "parse long" in {
      new LongParser[IO]().parse("1") === SuccessResponse[IO, Long](1)
    }
    "parse non-long" in {
      new LongParser[IO]().parse("12345678901234567890") must haveClass[FailureResponse[IO]]
    }

    "parse short" in {
      new ShortParser[IO]().parse("1") === SuccessResponse[IO, Short](1)
    }
    "parse non-short" in {
      new ShortParser[IO]().parse("12345678") must haveClass[FailureResponse[IO]]
    }

    "parse date" in {
      new DateParser[IO]().parse("2018-02-09") === SuccessResponse[IO, Date](
        new SimpleDateFormat("yyyy-MM-dd").parse("2018-02-09"))
    }

    "parse non-date" in {
      new DateParser[IO]().parse("2018-09") must haveClass[FailureResponse[IO]]
    }

    "parse instant" in {
      new InstantParser[IO]().parse("2018-02-09T00:00:00Z") === SuccessResponse[IO, Instant](
        Instant.parse("2018-02-09T00:00:00Z"))
    }

    "parse non-instant" in {
      new InstantParser[IO]().parse("2018-02-09 00:00:00Z") must haveClass[FailureResponse[IO]]
    }

    "parse uuid" in {
      new UUIDParser[IO]().parse("459043db-c29e-4dd9-a36d-b3a11b5eeb17") === SuccessResponse[IO, UUID](
        UUID.fromString("459043db-c29e-4dd9-a36d-b3a11b5eeb17"))
    }

    "parse non-uuid" in {
      new UUIDParser[IO]().parse("459043b-4dd9-a36d-b3a11b5eeb17") must haveClass[FailureResponse[IO]]
    }
  }

  "Mapped StringParser" should {
    val mappedParser = new LongParser[IO].map(UserId)

    "succeed when base parser has succeeded" in {
      mappedParser.parse("123") === SuccessResponse[IO, UserId](UserId(123L))
    }

    "fail when base parser has failed" in {
      mappedParser.parse("abc") must haveClass[FailureResponse[IO]]
    }
  }

  "RMapped StringParser" should {

    val rmappedParser = new IntParser[IO].rmap(i =>
      if(i >= 0) SuccessResponse(i)
      else FailureResponseOps[IO].badRequest("Only non-negative integers are accepted.")
    )

    "succeed when base parser has succeeded and the mapping function has returned a SuccessResponse" in {
      rmappedParser.parse("1") === SuccessResponse[IO, Int](1)
    }

    "fail when base parser has failed" in {
      rmappedParser.parse("abc") must haveClass[FailureResponse[IO]]
    }

    "fail when mapping function has returned a FailureResponse" in {
      rmappedParser.parse("-1") must haveClass[FailureResponse[IO]]
    }
  }
}

object StringParserSpec {
  case class UserId(id: Long)
}
