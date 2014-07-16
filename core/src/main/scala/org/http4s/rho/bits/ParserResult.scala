package org.http4s.rho.bits


sealed trait ParserResult[+T] {
  def map[T2](f: T => T2): ParserResult[T2] = this match {
    case ParseSuccess(v)              => ParseSuccess(f(v))
    case e@ ParseFailure(_)      => e
    case e@ ValidationFailure(_) => e
  }

  def flatMap[T2](f: T => ParserResult[T2]): ParserResult[T2] = this match {
    case ParseSuccess(v)              => f(v)
    case e@ ParseFailure(_)      => e
    case e@ ValidationFailure(_) => e
  }
}

case class ParseSuccess[+T](result: T) extends ParserResult[T]
case class ParseFailure(reason: String) extends ParserResult[Nothing]
case class ValidationFailure(reason: String) extends ParserResult[Nothing]
