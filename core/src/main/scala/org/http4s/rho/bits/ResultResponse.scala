package org.http4s.rho.bits

import org.http4s.rho.bits.FailureResponse._
import org.http4s.rho.bits.ResponseGeneratorInstances.BadRequest
import org.http4s.Response
import org.http4s.rho.Result.BaseResult

import scalaz.concurrent.Task

sealed trait RouteResult[+T] {
  final def isSuccess: Boolean = this match {
    case SuccessResponse(_) => true
    case _                => false
  }

  final def isEmpty: Boolean = this match {
    case NoMatch => true
    case _       => false
  }
}

case object NoMatch extends RouteResult[Nothing]

sealed trait ResultResponse[+T] extends RouteResult[T] {
  def map[T2](f: T => T2): ResultResponse[T2] = this match {
    case SuccessResponse(v)        => SuccessResponse(f(v))
    case e@ FailureResponse(_)     => e
  }

  def flatMap[T2](f: T => ResultResponse[T2]): ResultResponse[T2] = this match {
    case SuccessResponse(v)        => f(v)
    case e@ FailureResponse(_)     => e
  }

  def orElse[T2 >: T](other: => ResultResponse[T2]): ResultResponse[T2] = this match {
    case s @ SuccessResponse(_) => s
    case _                    => other
  }
}

case class SuccessResponse[+T](result: T) extends ResultResponse[T]

case class FailureResponse(reason: FailureReason) extends ResultResponse[Nothing] {
  def toResponse: Task[Response] = reason.toResponse
}

object FailureResponse {
  def badRequest(reason: String): FailureResponse = FailureResponse(new ResponseReason(BadRequest.pure(reason)))

  def pure(response: =>Task[Response]): FailureResponse = FailureResponse(new ResponseReason(response))

  def result(result: =>Task[BaseResult]): FailureResponse = pure(result.map(_.resp))

  trait FailureReason {
    def toResponse: Task[Response]
  }
  
  class ResponseReason(response: =>Task[Response]) extends FailureReason {
    def toResponse = response
  }
}
