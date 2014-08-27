package org.http4s.rho
package bits

import org.http4s.Header.`Content-Length`
import org.http4s.Writable.Entity
import org.http4s._

import scalaz.concurrent.Task

trait ResponseGenerator {
  def status: Status
}

abstract class EmptyResponseGenerator[S <: Status](val status: Status) extends ResponseGenerator {
  private val result: Task[Result[S, EmptyResult]] = Task.now(Result(Response(status)))
  def apply(): Task[Result[S, EmptyResult]] = result
}

abstract class EntityResponseGenerator[S <: Status](val status: Status) extends ResponseGenerator {
  def apply[A](body: A)(implicit w: Writable[A]): Task[Result[S, A]] =
    apply(body, Headers.empty)(w)

  def apply[A](body: A, headers: Headers)(implicit w: Writable[A]): Task[Result[S, A]] = {
    w.toEntity(body).flatMap { case Entity(proc, len) =>
      val hs = len match {
        case Some(l) => (w.headers ++ headers).put(`Content-Length`(l))
        case None    => (w.headers ++ headers)
      }
      Task.now(Result(Response(status = status, headers = hs, body = proc)))
    }
  }
}

abstract class LocationResponseGenerator[S <: Status](val status: Status) extends ResponseGenerator {
  def apply(location: Uri): Task[Result[S, EmptyResult]] =
    Task.now(Result(Response(status).putHeaders(Header.Location(location))))
}

object ResponseGeneratorInstances extends ResponseGeneratorInstances

trait ResponseGeneratorInstances {

  object Continue extends EmptyResponseGenerator[Status.Continue.type](Status.Continue)

  // TODO support Upgrade header
  object SwitchingProtocols extends EmptyResponseGenerator[Status.Ok.type](Status.SwitchingProtocols)

  object Ok extends EntityResponseGenerator[Status.Ok.type](Status.Ok)

  object Created extends EntityResponseGenerator[Status.Created.type](Status.Created)

  object Accepted extends EntityResponseGenerator[Status.Accepted.type](Status.Accepted)

  object NonAuthoritativeInformation extends EntityResponseGenerator[Status.NonAuthoritativeInformation.type](Status.NonAuthoritativeInformation)

  object NoContent extends EmptyResponseGenerator[Status.NoContent.type](Status.NoContent)

  object ResetContent extends EmptyResponseGenerator[Status.ResetContent.type](Status.ResetContent)

  // TODO helpers for Content-Range and multipart/byteranges
  object PartialContent extends EntityResponseGenerator[Status.PartialContent.type](Status.PartialContent)

  object MultiStatus extends EntityResponseGenerator[Status.MultiStatus.type](Status.MultiStatus)

  object AlreadyReported extends EntityResponseGenerator[Status.AlreadyReported.type](Status.AlreadyReported)

  object IMUsed extends EntityResponseGenerator[Status.IMUsed.type](Status.IMUsed)

  object MultipleChoices extends LocationResponseGenerator[Status.MultipleChoices.type](Status.MultipleChoices)

  object MovedPermanently extends LocationResponseGenerator[Status.MovedPermanently.type](Status.MovedPermanently)

  object Found extends LocationResponseGenerator[Status.Found.type](Status.Found)

  object SeeOther extends LocationResponseGenerator[Status.SeeOther.type](Status.SeeOther)

  object NotModified extends EntityResponseGenerator[Status.NotModified.type](Status.NotModified)

  // Note: UseProxy is deprecated in RFC7231, so we will not ease its creation here.

  object TemporaryRedirect extends LocationResponseGenerator[Status.TemporaryRedirect.type](Status.TemporaryRedirect)

  object PermanentRedirect extends LocationResponseGenerator[Status.PermanentRedirect.type](Status.PermanentRedirect)

  object BadRequest extends EntityResponseGenerator[Status.BadRequest.type](Status.BadRequest)

  object Unauthorized extends ResponseGenerator {
    val status: Status = Status.Unauthorized
    def apply(challenge: Challenge, challenges: Challenge*): Task[Result[Status.Unauthorized.type, EmptyResult]] =
      Task.now(Result(Response(status).putHeaders(Header.`WWW-Authenticate`(challenge, challenges: _*))))
  }

  object PaymentRequired extends EntityResponseGenerator[Status.PaymentRequired.type](Status.PaymentRequired)

  object Forbidden extends EntityResponseGenerator[Status.Forbidden.type](Status.Forbidden)

  object NotFound extends EntityResponseGenerator[Status.NotFound.type](Status.NotFound)

  object MethodNotAllowed extends ResponseGenerator {
    val status: Status = Status.MethodNotAllowed
    def apply(method: Method*): Task[Result[Status.MethodNotAllowed.type, EmptyResult]] = Task.now {
      Result(Response(status).putHeaders(Header.Raw("Allow".ci, method.mkString(","))))
    }
  }

  object NotAcceptable extends EntityResponseGenerator[Status.NotAcceptable.type](Status.NotAcceptable)

  object ProxyAuthenticationRequired extends EntityResponseGenerator[Status.ProxyAuthenticationRequired.type](Status.ProxyAuthenticationRequired)

  // TODO send Connection: close?
  object RequestTimeout extends EntityResponseGenerator[Status.RequestTimeout.type](Status.RequestTimeout)

  object Conflict extends EntityResponseGenerator[Status.Conflict.type](Status.Conflict)

  object Gone extends EntityResponseGenerator[Status.Gone.type](Status.Gone)

  object LengthRequired extends EntityResponseGenerator[Status.LengthRequired.type](Status.LengthRequired)

  object PreconditionFailed extends EntityResponseGenerator[Status.PreconditionFailed.type](Status.PreconditionFailed)

  object PayloadTooLarge extends EntityResponseGenerator[Status.PayloadTooLarge.type](Status.PayloadTooLarge)

  object UriTooLong extends EntityResponseGenerator[Status.UriTooLong.type](Status.UriTooLong)

  object UnsupportedMediaType extends EntityResponseGenerator[Status.UnsupportedMediaType.type](Status.UnsupportedMediaType)

  object RangeNotSatisfiable extends EntityResponseGenerator[Status.RangeNotSatisfiable.type](Status.RangeNotSatisfiable)

  object ExpectationFailed extends EntityResponseGenerator[Status.ExpectationFailed.type](Status.ExpectationFailed)

  object UnprocessableEntity extends EntityResponseGenerator[Status.UnprocessableEntity.type](Status.UnprocessableEntity)

  object Locked extends EntityResponseGenerator[Status.Locked.type](Status.Locked)

  object FailedDependency extends EntityResponseGenerator[Status.FailedDependency.type](Status.FailedDependency)

  // TODO Mandatory upgrade field
  object UpgradeRequired extends EntityResponseGenerator[Status.UpgradeRequired.type](Status.UpgradeRequired)

  object PreconditionRequired extends EntityResponseGenerator[Status.PreconditionFailed.type](Status.PreconditionRequired)

  object TooManyRequests extends EntityResponseGenerator[Status.TooManyRequests.type](Status.TooManyRequests)

  object RequestHeaderFieldsTooLarge extends EntityResponseGenerator[Status.RequestHeaderFieldsTooLarge.type](Status.RequestHeaderFieldsTooLarge)

  object InternalServerError extends EntityResponseGenerator[Status.InternalServerError.type](Status.InternalServerError)

  object NotImplemented extends EntityResponseGenerator[Status.NotImplemented.type](Status.NotImplemented)

  object BadGateway extends EntityResponseGenerator[Status.BadGateway.type](Status.BadGateway)

  object ServiceUnavailable extends EntityResponseGenerator[Status.ServiceUnavailable.type](Status.ServiceUnavailable)

  object GatewayTimeout extends EntityResponseGenerator[Status.GatewayTimeout.type](Status.GatewayTimeout)

  object HttpVersionNotSupported extends EntityResponseGenerator[Status.HttpVersionNotSupported.type](Status.HttpVersionNotSupported)

  object VariantAlsoNegotiates extends EntityResponseGenerator[Status.VariantAlsoNegotiates.type](Status.VariantAlsoNegotiates)

  object InsufficientStorage extends EntityResponseGenerator[Status.InsufficientStorage.type](Status.InsufficientStorage)

  object LoopDetected extends EntityResponseGenerator[Status.LoopDetected.type](Status.LoopDetected)

  object NotExtended extends EntityResponseGenerator[Status.NotExtended.type](Status.NotExtended)

  object NetworkAuthenticationRequired extends EntityResponseGenerator[Status.NetworkAuthenticationRequired.type](Status.NetworkAuthenticationRequired)
}
