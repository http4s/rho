package org.http4s.rho.bits

import org.http4s.rho.Result
import org.http4s.rho.bits.ParserFailure._
import org.http4s.rho.bits.ResponseGeneratorInstances.BadRequest
import org.http4s.{Status, Response}
import org.http4s.rho.Result.BaseResult

import scalaz.concurrent.Task

sealed trait RouteResult[+T] {
  final def isSuccess: Boolean = this match {
    case ParserSuccess(_) => true
    case _                => false
  }

  final def isEmpty: Boolean = this match {
    case NoMatch => true
    case _       => false
  }
}

case object NoMatch extends RouteResult[Nothing]

sealed trait ParserResult[+T] extends RouteResult[T] {
  def map[T2](f: T => T2): ParserResult[T2] = this match {
    case ParserSuccess(v)        => ParserSuccess(f(v))
    case e@ ParserFailure(_)     => e
  }

  def flatMap[T2](f: T => ParserResult[T2]): ParserResult[T2] = this match {
    case ParserSuccess(v)        => f(v)
    case e@ ParserFailure(_)     => e
  }

  def orElse[T2 >: T](other: => ParserResult[T2]): ParserResult[T2] = this match {
    case s @ ParserSuccess(_) => s
    case _                    => other
  }
}

case class ParserSuccess[+T](result: T) extends ParserResult[T]

case class ParserFailure(reason: FailureReason) extends ParserResult[Nothing] {
  def toResponse: Task[Response] = reason.toResponse
}

object ParserFailure {
  def badRequest(reason: String): ParserFailure = ParserFailure(ResponseReason(BadRequest.pure(reason)))

  def pure(response: Task[Response]): ParserFailure = ParserFailure(ResponseReason(response))

  def result(result: Task[BaseResult]): ParserFailure = pure(result.map(_.resp))

  trait FailureReason {
    def toResponse: Task[Response]
  }
  
  case class ResponseReason(response: Task[Response]) extends FailureReason {
    def toResponse = response
  }
}
