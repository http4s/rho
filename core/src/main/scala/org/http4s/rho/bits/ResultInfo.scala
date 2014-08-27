package org.http4s.rho.bits

import org.http4s.Status
import scala.reflect.runtime.universe.{ Type, typeOf }


sealed trait ResultInfo
case class ModelOnly(tpe: Type) extends ResultInfo
case class StatusAndModel(status: Status, tpe: Type) extends ResultInfo
case class StatusOnly(status: Status) extends ResultInfo
case object Empty extends ResultInfo

object ResultInfo {
  private lazy val statusIndex: Set[(Type, Status)] = Set(
    (typeOf[Status.NotExtended.type], Status.NotExtended),
    (typeOf[Status.NetworkAuthenticationRequired.type], Status.NetworkAuthenticationRequired),
    (typeOf[Status.LoopDetected.type], Status.LoopDetected),
    (typeOf[Status.InsufficientStorage.type], Status.InsufficientStorage),
    (typeOf[Status.VariantAlsoNegotiates.type], Status.VariantAlsoNegotiates),
    (typeOf[Status.HttpVersionNotSupported.type], Status.HttpVersionNotSupported),
    (typeOf[Status.GatewayTimeout.type], Status.GatewayTimeout),
    (typeOf[Status.ServiceUnavailable.type], Status.ServiceUnavailable),
    (typeOf[Status.BadGateway.type], Status.BadGateway),
    (typeOf[Status.NotImplemented.type], Status.NotImplemented),
    (typeOf[Status.Locked.type], Status.Locked),
    (typeOf[Status.InternalServerError.type], Status.InternalServerError),
    (typeOf[Status.RequestHeaderFieldsTooLarge.type], Status.RequestHeaderFieldsTooLarge),
    (typeOf[Status.TooManyRequests.type], Status.TooManyRequests),
    (typeOf[Status.PreconditionRequired.type], Status.PreconditionRequired),
    (typeOf[Status.UpgradeRequired.type], Status.UpgradeRequired),
    (typeOf[Status.FailedDependency.type], Status.FailedDependency),
    (typeOf[Status.UnprocessableEntity.type], Status.UnprocessableEntity),
    (typeOf[Status.ExpectationFailed.type], Status.ExpectationFailed),
    (typeOf[Status.RangeNotSatisfiable.type], Status.RangeNotSatisfiable),
    (typeOf[Status.UnsupportedMediaType.type], Status.UnsupportedMediaType),
    (typeOf[Status.UriTooLong.type], Status.UriTooLong),
    (typeOf[Status.PayloadTooLarge.type], Status.PayloadTooLarge),
    (typeOf[Status.PreconditionFailed.type], Status.PreconditionFailed),
    (typeOf[Status.LengthRequired.type], Status.LengthRequired),
    (typeOf[Status.Gone.type], Status.Gone),
    (typeOf[Status.Conflict.type], Status.Conflict),
    (typeOf[Status.RequestTimeout.type], Status.RequestTimeout),
    (typeOf[Status.ProxyAuthenticationRequired.type], Status.ProxyAuthenticationRequired),
    (typeOf[Status.NotAcceptable.type], Status.NotAcceptable),
    (typeOf[Status.MethodNotAllowed.type], Status.MethodNotAllowed),
    (typeOf[Status.NotFound.type], Status.NotFound),
    (typeOf[Status.Forbidden.type], Status.Forbidden),
    (typeOf[Status.PaymentRequired.type], Status.PaymentRequired),
    (typeOf[Status.Unauthorized.type], Status.Unauthorized),
    (typeOf[Status.BadRequest.type], Status.BadRequest),
    (typeOf[Status.PermanentRedirect.type], Status.PermanentRedirect),
    (typeOf[Status.TemporaryRedirect.type], Status.TemporaryRedirect),
    (typeOf[Status.UseProxy.type], Status.UseProxy),
    (typeOf[Status.NotModified.type], Status.NotModified),
    (typeOf[Status.SeeOther.type], Status.SeeOther),
    (typeOf[Status.MovedPermanently.type], Status.MovedPermanently),
    (typeOf[Status.MultipleChoices.type], Status.MultipleChoices),
    (typeOf[Status.IMUsed.type], Status.IMUsed),
    (typeOf[Status.AlreadyReported.type], Status.AlreadyReported),
    (typeOf[Status.MultiStatus.type], Status.MultiStatus),
    (typeOf[Status.PartialContent.type], Status.PartialContent),
    (typeOf[Status.ResetContent.type], Status.ResetContent),
    (typeOf[Status.NoContent.type], Status.NoContent),
    (typeOf[Status.NonAuthoritativeInformation.type], Status.NonAuthoritativeInformation),
    (typeOf[Status.Accepted.type], Status.Accepted),
    (typeOf[Status.Created.type], Status.Created),
    (typeOf[Status.Ok.type], Status.Ok),
    (typeOf[Status.Processing.type], Status.Processing),
    (typeOf[Status.SwitchingProtocols.type], Status.SwitchingProtocols),
    (typeOf[Status.Continue.type], Status.Continue)
  )
  
  def getStatus(tpe: Type): Option[Status] = statusIndex.find{ case (t2, s) => tpe =:= t2 }.map(_._2)
}