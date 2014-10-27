package org.http4s.rho.bits


sealed trait RouteResult[+T]

case object NoMatch extends RouteResult[Nothing]

sealed trait ParserResult[+T] extends RouteResult[T] {
  def map[T2](f: T => T2): ParserResult[T2] = this match {
    case ParserSuccess(v)        => ParserSuccess(f(v))
    case e@ ParserFailure(_)     => e
    case e@ ValidationFailure(_) => e
  }

  def flatMap[T2](f: T => ParserResult[T2]): ParserResult[T2] = this match {
    case ParserSuccess(v)        => f(v)
    case e@ ParserFailure(_)     => e
    case e@ ValidationFailure(_) => e
  }

  def orElse[T2 >: T](other: => ParserResult[T2]): ParserResult[T2] = this match {
    case s @ ParserSuccess(_) => s
    case _                    => other
  }
}

case class ParserSuccess[+T](result: T) extends ParserResult[T]
case class ParserFailure(reason: String) extends ParserResult[Nothing]
case class ValidationFailure(reason: String) extends ParserResult[Nothing]
