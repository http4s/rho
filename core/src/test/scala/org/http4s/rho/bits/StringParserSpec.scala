//package org.http4s.rho.bits
//
//import org.specs2.mutable.Specification
//
//class StringParserSpec extends Specification {
//
//  "StringParser" should {
//
//    "parse true" in {
//      new BooleanParser().parse("true") == ParserSuccess(true)
//    }
//    "parse false" in {
//      new BooleanParser().parse("false") == ParserSuccess(false)
//    }
//    "not parse non-boolean" in {
//      new BooleanParser().parse("unknown") must haveClass[ParserFailure]
//    }
//
//    "parse double" in {
//      new DoubleParser().parse("1.1") == ParserSuccess[Double](1.1)
//    }
//    "parse non-double" in {
//      new DoubleParser().parse("unknown") must haveClass[ParserFailure]
//    }
//
//    "parse float" in {
//      new FloatParser().parse("1.1") == ParserSuccess[Float](1.1f)
//    }
//    "parse non-float" in {
//      new FloatParser().parse("unknown") must haveClass[ParserFailure]
//    }
//
//    "parse int" in {
//      new IntParser().parse("1") == ParserSuccess[Int](1)
//    }
//    "parse non-int" in {
//      new IntParser().parse("12345678901") must haveClass[ParserFailure]
//    }
//
//    "parse long" in {
//      new LongParser().parse("1") == ParserSuccess[Long](1)
//    }
//    "parse non-long" in {
//      new LongParser().parse("12345678901234567890") must haveClass[ParserFailure]
//    }
//
//    "parse short" in {
//      new ShortParser().parse("1") == ParserSuccess[Short](1)
//    }
//    "parse non-short" in {
//      new ShortParser().parse("12345678") must haveClass[ParserFailure]
//    }
//
//  }
//
//}
