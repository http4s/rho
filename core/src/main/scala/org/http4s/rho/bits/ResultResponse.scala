package org.http4s.rho.bits

import cats.data.OptionT
import org.http4s._
import org.http4s.rho.Result.BaseResult
import org.http4s.rho.bits.FailureResponse._
import org.http4s.rho.bits.ResponseGeneratorInstances.{BadRequest, InternalServerError}
import cats.effect.IO

/** Types that represent the result of executing a step of the route */
sealed trait RouteResult[+T] {
  final def isSuccess: Boolean = this match {
    case SuccessResponse(_) => true
    case _                => false
  }

  final def isEmpty: Boolean = this match {
    case NoMatch => true
    case _       => false
  }

  final def toResponse(implicit ev: T <:< IO[Response[IO]]): OptionT[IO, Response[IO]] = this match {
      case SuccessResponse(t) => OptionT.liftF(t)
      case NoMatch            => OptionT.none
      case FailureResponse(r) => OptionT.liftF(r.toResponse)
    }
}

/** Failure to match a route */
case object NoMatch extends RouteResult[Nothing]

/** Node in the ADT that represents a result, either success or failure */
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

/** Successful response */
final case class SuccessResponse[+T](result: T) extends ResultResponse[T]

/** Response that signifies an error
  *
  * @param reason The reason for failure which can be turned into a `IO[Response[IO]`.
  */
final case class FailureResponse(reason: ResponseReason) extends ResultResponse[Nothing] {
  def toResponse: IO[Response[IO]]= reason.toResponse
}

object FailureResponse {

  /** Construct a `400 BadRequest` FailureResponse
    *
    * @param reason Description of the failure
    */
  def badRequest[T](reason: T)(implicit w: EntityEncoder[IO, T]): FailureResponse = FailureResponse(new ResponseReason(BadRequest.pure(reason)))

  /** Construct a `500 InternalServerError` FailureResponse
    *
    * @param reason Description of the failure
    */
  def error[T](reason: T)(implicit w: EntityEncoder[IO, T]): FailureResponse = FailureResponse(new ResponseReason(InternalServerError.pure(reason)))

  /** Construct a [[FailureResponse]] using the provided thunk. */
  def pure(response: =>IO[Response[IO]]): FailureResponse = FailureResponse(new ResponseReason(response))

  /** Construct a [[FailureResponse]] using the provided thunk. */
  def result(result: =>IO[BaseResult]): FailureResponse = pure(result.map(_.resp))

  /** Concrete representation of the `FailureResponse` */
  final class ResponseReason(response: =>IO[Response[IO]]) {
    def toResponse = response
  }
}
