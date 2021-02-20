package org.http4s.rho.bits

import cats.data.OptionT
import cats.{Applicative, Functor, Monad}
import cats.syntax.functor._
import org.http4s._
import org.http4s.rho.Result.BaseResult

/** Types that represent the result of executing a step of the route */
sealed trait RouteResult[F[_], +T] {
  final def isSuccess: Boolean = this match {
    case SuccessResponse(_) => true
    case _ => false
  }

  final def isEmpty: Boolean = this match {
    case NoMatch() => true
    case _ => false
  }

  final def toResponse(implicit
      F: Applicative[F],
      ev: T <:< F[Response[F]]): OptionT[F, Response[F]] =
    this match {
      case SuccessResponse(t) => OptionT(ev(t).map(Option.apply))
      case NoMatch() => OptionT.none[F, Response[F]]
      case FailureResponse(r) => OptionT(r.toResponse.map(Option.apply))
    }
}

/** Failure to match a route */
case class NoMatch[F[_]]() extends RouteResult[F, Nothing]

/** Node in the ADT that represents a result, either success or failure */
sealed trait ResultResponse[F[_], +T] extends RouteResult[F, T] {
  def map[T2](f: T => T2): ResultResponse[F, T2] = this match {
    case SuccessResponse(v) => SuccessResponse(f(v))
    case e @ FailureResponse(_) => e
  }

  def flatMap[T2](f: T => ResultResponse[F, T2]): ResultResponse[F, T2] = this match {
    case SuccessResponse(v) => f(v)
    case e @ FailureResponse(_) => e
  }

  def orElse[T2 >: T](other: => ResultResponse[F, T2]): ResultResponse[F, T2] = this match {
    case s @ SuccessResponse(_) => s
    case _ => other
  }
}

/** Successful response */
final case class SuccessResponse[F[_], +T](result: T) extends ResultResponse[F, T]

/** Response that signifies an error
  *
  * @param reason The reason for failure which can be turned into a `F[Response[F]]`.
  */
final case class FailureResponse[F[_]](reason: ResponseReason[F])
    extends ResultResponse[F, Nothing] {
  def toResponse: F[Response[F]] = reason.toResponse
}

object FailureResponse {

  /** Construct a [[FailureResponse]] using the provided thunk. */
  def pure[F[_]](response: => F[Response[F]]): FailureResponse[F] = FailureResponse(
    new ResponseReason(response)
  )

  /** Construct a [[FailureResponse]] using the provided thunk. */
  def result[F[_]](result: => F[BaseResult[F]])(implicit F: Functor[F]): FailureResponse[F] = pure(
    F.map(result)(_.resp)
  )
}

trait FailureResponseOps[F[_]] extends ResponseGeneratorInstances[F] {

  /** Construct a `400 BadRequest` FailureResponse
    *
    * @param reason Description of the failure
    */
  def badRequest[T](reason: T)(implicit F: Monad[F], w: EntityEncoder[F, T]): FailureResponse[F] =
    FailureResponse[F](new ResponseReason(BadRequest.pure(reason)))

  /** Construct a `500 InternalServerError` FailureResponse
    *
    * @param reason Description of the failure
    */
  def error[T](reason: T)(implicit F: Monad[F], w: EntityEncoder[F, T]): FailureResponse[F] =
    FailureResponse[F](new ResponseReason(InternalServerError.pure(reason)))
}
object FailureResponseOps {
  def apply[F[_]]: FailureResponseOps[F] = new FailureResponseOps[F] {}
}

/** Concrete representation of the `FailureResponse` */
final class ResponseReason[F[_]](response: => F[Response[F]]) {
  def toResponse: F[Response[F]] = response
}
