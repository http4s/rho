package org.http4s
package rho

import cats._
import org.http4s.headers.`Content-Type`

/** A helper for capturing the result types and status codes from routes */
sealed case class Result[
F[_],
+CONTINUE,
+SWITCHINGPROTOCOLS,
+PROCESSING,

+OK,
+CREATED,
+ACCEPTED,
+NONAUTHORITATIVEINFORMATION,
+NOCONTENT,
+RESETCONTENT,
+PARTIALCONTENT,
+MULTISTATUS,
+ALREADYREPORTED,
+IMUSED,

+MULTIPLECHOICES,
+MOVEDPERMANENTLY,
+FOUND,
+SEEOTHER,
+NOTMODIFIED,
+USEPROXY,
+TEMPORARYREDIRECT,
+PERMANENTREDIRECT,

+BADREQUEST,
+UNAUTHORIZED,
+PAYMENTREQUIRED,
+FORBIDDEN,
+NOTFOUND,
+METHODNOTALLOWED,
+NOTACCEPTABLE,
+PROXYAUTHENTICATIONREQUIRED,
+REQUESTTIMEOUT,
+CONFLICT,
+GONE,
+LENGTHREQUIRED,
+PRECONDITIONFAILED,
+PAYLOADTOOLARGE,
+URITOOLONG,
+UNSUPPORTEDMEDIATYPE,
+RANGENOTSATISFIABLE,
+EXPECTATIONFAILED,
+UNPROCESSABLEENTITY,
+LOCKED,
+FAILEDDEPENDENCY,
+UPGRADEREQUIRED,
+PRECONDITIONREQUIRED,
+TOOMANYREQUESTS,
+REQUESTHEADERFIELDSTOOLARGE,

+INTERNALSERVERERROR,
+NOTIMPLEMENTED,
+BADGATEWAY,
+SERVICEUNAVAILABLE,
+GATEWAYTIMEOUT,
+HTTPVERSIONNOTSUPPORTED,
+VARIANTALSONEGOTIATES,
+INSUFFICIENTSTORAGE,
+LOOPDETECTED,
+NOTEXTENDED,
+NETWORKAUTHENTICATIONREQUIRED
](resp: Response[F])

object Result {

  /** Result type with completely ambiguous return types */
  type BaseResult[F[_]] = Result[F, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]

  /** Result with no inferred return types */
  type TopResult[F[_]] = Result[F, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

  /** Existential result type */
  type ExResult[F[_]] = Result[F, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
}

trait ResultSyntaxInstances[F[_]] {
  implicit class ResultSyntax[R >: Result.TopResult[F] <: Result.BaseResult[F]](r: R) {
    type Self = R

    private val resp: Response[F] = r.resp

    //def mapK[G[_]](f: ~>[F, G]): R[G] = Result(resp.mapK(f)).asInstanceOf[R[G]]

    def withHttpVersion(httpVersion: HttpVersion): Self = Result(resp.withHttpVersion(httpVersion))

    def withHeaders(headers: Headers): Self = Result(resp.withHeaders(headers))

    def withHeaders(headers: Header*): Self = Result(resp.withHeaders(headers: _*))

    def withAttributes(attributes: AttributeMap): Self = Result(resp.withAttributes(attributes))

    def transformHeaders(f: Headers => Headers): Self = Result(resp.transformHeaders(f))

    def filterHeaders(f: Header => Boolean): Self = Result(resp.filterHeaders(f))

    def removeHeader(key: HeaderKey): Self = Result(resp.removeHeader(key))

    def putHeaders(headers: Header*): Self = Result(resp.putHeaders(headers: _*))

    @scala.deprecated("Use withHeaders instead", "0.20.0-M2")
    def replaceAllHeaders(headers: Headers): Self = Result(resp.replaceAllHeaders(headers))

    @scala.deprecated("Use withHeaders instead", "0.20.0-M2")
    def replaceAllHeaders(headers: Header*): Self = Result(resp.replaceAllHeaders(headers: _*))

    def withTrailerHeaders(trailerHeaders: F[Headers]): Self = Result(resp.withTrailerHeaders(trailerHeaders))

    def withoutTrailerHeaders: Self = Result(resp.withoutTrailerHeaders)

    def trailerHeaders(implicit F: Applicative[F]): F[Headers] = resp.trailerHeaders(F)

    @scala.deprecated("Use withContentType(`Content-Type`(t)) instead", "0.20.0-M2")
    def withType(t: MediaType)(implicit F: Functor[F]): Self = Result(resp.withType(t)(F))

    def withContentType(contentType: `Content-Type`): Self = Result(resp.withContentType(contentType))

    def withoutContentType: Self = Result(resp.withoutContentType)

    def withContentTypeOption(contentTypeO: Option[`Content-Type`]): Self = Result(resp.withContentTypeOption(contentTypeO))

    def contentType: Option[`Content-Type`] = resp.contentType

    def contentLength: Option[Long] = resp.contentLength

    def charset: Option[Charset] = resp.charset

    def isChunked: Boolean = resp.isChunked

    def withAttribute[A](key: AttributeKey[A], value: A): Self = Result(resp.withAttribute(key, value))

    def withAttribute[A](entry: AttributeEntry[A]): Self = Result(resp.withAttribute(entry))

    def withoutAttribute(key: AttributeKey[_]): Self = Result(resp.withoutAttribute(key))

    def attemptAs[T](implicit decoder: EntityDecoder[F, T]): DecodeResult[F, T] = resp.attemptAs(decoder)

    def as[T](implicit F: Functor[F], decoder: EntityDecoder[F, T]): F[T] = resp.as(F, decoder)

    def withStatus(status: Status): Self = Result(resp.withStatus(status))

    def addCookie(cookie: ResponseCookie): Self = Result(resp.addCookie(cookie))

    def addCookie(name: String, content: String, expires: Option[HttpDate] = None): Self = Result(resp.addCookie(name, content, expires))

    def removeCookie(cookie: ResponseCookie): Self = Result(resp.removeCookie(cookie))

    def removeCookie(name: String): Self = Result(resp.removeCookie(name))

    def cookies: List[ResponseCookie] = resp.cookies
  }
}
