package org.http4s.rho.bits

import java.text.SimpleDateFormat
import java.time.Instant
import java.util.{Date, UUID}

import cats.effect.IO
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
}
