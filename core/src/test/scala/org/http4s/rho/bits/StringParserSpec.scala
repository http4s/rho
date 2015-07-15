package org.http4s.rho.bits

import org.specs2.mutable.Specification

class StringParserSpec extends Specification {

  "StringParser" should {

    "parse true" in {
      new BooleanParser().parse("true") == SuccessResponse(true)
    }
    "parse false" in {
      new BooleanParser().parse("false") == SuccessResponse(false)
    }
    "not parse non-boolean" in {
      new BooleanParser().parse("unknown") must haveClass[FailureResponse]
    }

    "parse double" in {
      new DoubleParser().parse("1.1") == SuccessResponse[Double](1.1)
    }
    "parse non-double" in {
      new DoubleParser().parse("unknown") must haveClass[FailureResponse]
    }

    "parse float" in {
      new FloatParser().parse("1.1") == SuccessResponse[Float](1.1f)
    }
    "parse non-float" in {
      new FloatParser().parse("unknown") must haveClass[FailureResponse]
    }

    "parse int" in {
      new IntParser().parse("1") == SuccessResponse[Int](1)
    }
    "parse non-int" in {
      new IntParser().parse("12345678901") must haveClass[FailureResponse]
    }

    "parse long" in {
      new LongParser().parse("1") == SuccessResponse[Long](1)
    }
    "parse non-long" in {
      new LongParser().parse("12345678901234567890") must haveClass[FailureResponse]
    }

    "parse short" in {
      new ShortParser().parse("1") == SuccessResponse[Short](1)
    }
    "parse non-short" in {
      new ShortParser().parse("12345678") must haveClass[FailureResponse]
    }

  }

}
