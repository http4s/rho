package org.http4s.rho.bits

import org.http4s.Response
import org.http4s.rho.Result.BaseResult

import scalaz.concurrent.Task

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
// TODO: I think the reason for failure could be made easier to use with specific failure types
case class ValidationFailure(response: Task[BaseResult]) extends ParserResult[Nothing]

