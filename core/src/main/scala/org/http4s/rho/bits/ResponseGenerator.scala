package org.http4s.rho
package bits

import org.http4s.Header.`Content-Length`
import org.http4s.Writable.Entity
import org.http4s._
import org.http4s.util.CaseInsensitiveString._

import scalaz.concurrent.Task

trait ResponseGenerator {
  def status: Status
}

abstract class EmptyResponseGenerator(val status: Status) extends ResponseGenerator {
  private val result: Task[Result[EmptyResult]] = Task.now(Result(Response(status)))
  def apply(): Task[Result[EmptyResult]] = result
}

abstract class EntityResponseGenerator(val status: Status) extends ResponseGenerator {
  def apply[A](body: A)(implicit w: Writable[A]): Task[Result[A]] =
    apply(body, Headers.empty)(w)

  def apply[A](body: A, headers: Headers)(implicit w: Writable[A]): Task[Result[A]] = {
    w.toEntity(body).flatMap { case Entity(proc, len) =>
      val hs = len match {
        case Some(l) => headers ++ w.headers.put(`Content-Length`(l))
        case None    => headers ++ w.headers
      }
      Task.now(Result(Response(status = status, headers = hs, body = proc)))
    }
  }
}

abstract class LocationResponseGenerator(val status: Status) extends ResponseGenerator {
  def apply(location: Uri): Task[Result[EmptyResult]] =
    Task.now(Result(Response(status).putHeaders(Header.Location(location))))
}

object ResponseGeneratorInstances extends ResponseGeneratorInstances

trait ResponseGeneratorInstances {

  object Continue extends EmptyResponseGenerator(Status.Continue)

  // TODO support Upgrade header
  object SwitchingProtocols extends EmptyResponseGenerator(Status.SwitchingProtocols)

  object Ok extends EntityResponseGenerator(Status.Ok)

  object Created extends EntityResponseGenerator(Status.Created)

  object Accepted extends EntityResponseGenerator(Status.Accepted)

  object NonAuthoritativeInformation extends EntityResponseGenerator(Status.NonAuthoritativeInformation)

  object NoContent extends EmptyResponseGenerator(Status.NoContent)

  object ResetContent extends EmptyResponseGenerator(Status.ResetContent)

  // TODO helpers for Content-Range and multipart/byteranges
  object PartialContent extends EntityResponseGenerator(Status.PartialContent)

  object MultiStatus extends EntityResponseGenerator(Status.MultiStatus)

  object AlreadyReported extends EntityResponseGenerator(Status.AlreadyReported)

  object IMUsed extends EntityResponseGenerator(Status.IMUsed)

  object MultipleChoices extends LocationResponseGenerator(Status.MultipleChoices)

  object MovedPermanently extends LocationResponseGenerator(Status.MovedPermanently)

  object Found extends LocationResponseGenerator(Status.Found)

  object SeeOther extends LocationResponseGenerator(Status.SeeOther)

  object NotModified extends EntityResponseGenerator(Status.NotModified)

  // Note: UseProxy is deprecated in RFC7231, so we will not ease its creation here.

  object TemporaryRedirect extends LocationResponseGenerator(Status.TemporaryRedirect)

  object PermanentRedirect extends LocationResponseGenerator(Status.PermanentRedirect)

  object BadRequest extends EntityResponseGenerator(Status.BadRequest)

  object Unauthorized extends ResponseGenerator {
    val status: Status = Status.Unauthorized
    def apply(challenge: Challenge, challenges: Challenge*): Task[Result[EmptyResult]] =
      Task.now(Result(Response(status).putHeaders(Header.`WWW-Authenticate`(challenge, challenges: _*))))
  }

  object PaymentRequired extends EntityResponseGenerator(Status.PaymentRequired)

  object Forbidden extends EntityResponseGenerator(Status.Forbidden)

  object NotFound extends EntityResponseGenerator(Status.NotFound)

  object MethodNotAllowed extends ResponseGenerator {
    val status: Status = Status.MethodNotAllowed
    def apply(method: Method*): Task[Result[EmptyResult]] = Task.now {
      Result(Response(status).putHeaders(Header.Raw("Allow".ci, method.mkString(","))))
    }
  }

  object NotAcceptable extends EntityResponseGenerator(Status.NotAcceptable)

  object ProxyAuthenticationRequired extends EntityResponseGenerator (Status.ProxyAuthenticationRequired)

  // TODO send Connection: close?
  object RequestTimeout extends EntityResponseGenerator(Status.RequestTimeout)

  object Conflict extends EntityResponseGenerator(Status.Conflict)

  object Gone extends EntityResponseGenerator(Status.Gone)

  object LengthRequired extends EntityResponseGenerator(Status.LengthRequired)

  object PreconditionFailed extends EntityResponseGenerator(Status.PreconditionFailed)

  object PayloadTooLarge extends EntityResponseGenerator(Status.PayloadTooLarge)

  object UriTooLong extends EntityResponseGenerator(Status.UriTooLong)

  object UnsupportedMediaType extends EntityResponseGenerator(Status.UnsupportedMediaType)

  object RangeNotSatisfiable extends EntityResponseGenerator(Status.RangeNotSatisfiable)

  object ExpectationFailed extends EntityResponseGenerator(Status.ExpectationFailed)

  object UnprocessableEntity extends EntityResponseGenerator(Status.UnprocessableEntity)

  object Locked extends EntityResponseGenerator(Status.Locked)

  object FailedDependency extends EntityResponseGenerator(Status.FailedDependency)

  // TODO Mandatory upgrade field
  object UpgradeRequired extends EntityResponseGenerator(Status.UpgradeRequired)

  object PreconditionRequired extends EntityResponseGenerator(Status.PreconditionRequired)

  object TooManyRequests extends EntityResponseGenerator(Status.TooManyRequests)

  object RequestHeaderFieldsTooLarge extends EntityResponseGenerator(Status.RequestHeaderFieldsTooLarge)

  object InternalServerError extends EntityResponseGenerator(Status.InternalServerError)

  object NotImplemented extends EntityResponseGenerator(Status.NotImplemented)

  object BadGateway extends EntityResponseGenerator(Status.BadGateway)

  object ServiceUnavailable extends EntityResponseGenerator(Status.ServiceUnavailable)

  object GatewayTimeout extends EntityResponseGenerator(Status.GatewayTimeout)

  object HttpVersionNotSupported extends EntityResponseGenerator(Status.HttpVersionNotSupported)

  object VariantAlsoNegotiates extends EntityResponseGenerator (Status.VariantAlsoNegotiates)

  object InsufficientStorage extends EntityResponseGenerator(Status.InsufficientStorage)

  object LoopDetected extends EntityResponseGenerator(Status.LoopDetected)

  object NotExtended extends EntityResponseGenerator(Status.NotExtended)

  object NetworkAuthenticationRequired extends EntityResponseGenerator(Status.NetworkAuthenticationRequired)
}
