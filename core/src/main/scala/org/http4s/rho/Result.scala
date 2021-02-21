package org.http4s
package rho

import cats._
import org.http4s.headers.`Content-Type`
import org.typelevel.vault._

/** A helper for capturing the result types and status codes from routes */
sealed case class Result[
    F[_],
    // Informational
    /* 100 */ +CONTINUE,
    /* 101 */ +SWITCHINGPROTOCOLS,
    /* 102 */ +PROCESSING,
    /* 103 */ +EARLYHINTS,
    // Successful
    /* 200 */ +OK,
    /* 201 */ +CREATED,
    /* 202 */ +ACCEPTED,
    /* 203 */ +NONAUTHORITATIVEINFORMATION,
    /* 204 */ +NOCONTENT,
    /* 205 */ +RESETCONTENT,
    /* 206 */ +PARTIALCONTENT,
    /* 207 */ +MULTISTATUS,
    /* 208 */ +ALREADYREPORTED,
    /* 226 */ +IMUSED,
    // Redirects
    /* 300 */ +MULTIPLECHOICES,
    /* 301 */ +MOVEDPERMANENTLY,
    /* 302 */ +FOUND,
    /* 303 */ +SEEOTHER,
    /* 304 */ +NOTMODIFIED,
    /* 305 */ /* USEPROXY is deprecated https://tools.ietf.org/html/rfc7231#section-6.4.5*/
    /* 307 */ +TEMPORARYREDIRECT,
    /* 308 */ +PERMANENTREDIRECT,
    // Client errors
    /* 400 */ +BADREQUEST,
    /* 401 */ +UNAUTHORIZED,
    /* 402 */ +PAYMENTREQUIRED,
    /* 403 */ +FORBIDDEN,
    /* 404 */ +NOTFOUND,
    /* 405 */ +METHODNOTALLOWED,
    /* 406 */ +NOTACCEPTABLE,
    /* 407 */ +PROXYAUTHENTICATIONREQUIRED,
    /* 408 */ +REQUESTTIMEOUT,
    /* 409 */ +CONFLICT,
    /* 410 */ +GONE,
    /* 411 */ +LENGTHREQUIRED,
    /* 412 */ +PRECONDITIONFAILED,
    /* 413 */ +PAYLOADTOOLARGE,
    /* 414 */ +URITOOLONG,
    /* 415 */ +UNSUPPORTEDMEDIATYPE,
    /* 416 */ +RANGENOTSATISFIABLE,
    /* 417 */ +EXPECTATIONFAILED,
    /* 421 */ +MISDIRECTEDREQUEST,
    /* 422 */ +UNPROCESSABLEENTITY,
    /* 423 */ +LOCKED,
    /* 424 */ +FAILEDDEPENDENCY,
    /* 424 */ +TOOEARLY,
    /* 426 */ +UPGRADEREQUIRED,
    /* 428 */ +PRECONDITIONREQUIRED,
    /* 429 */ +TOOMANYREQUESTS,
    /* 431 */ +REQUESTHEADERFIELDSTOOLARGE,
    /* 451 */ +UNAVAILABLEFORLEGALREASONS,
    // Server errors
    /* 500 */ +INTERNALSERVERERROR,
    /* 501 */ +NOTIMPLEMENTED,
    /* 502 */ +BADGATEWAY,
    /* 503 */ +SERVICEUNAVAILABLE,
    /* 504 */ +GATEWAYTIMEOUT,
    /* 505 */ +HTTPVERSIONNOTSUPPORTED,
    /* 506 */ +VARIANTALSONEGOTIATES,
    /* 507 */ +INSUFFICIENTSTORAGE,
    /* 508 */ +LOOPDETECTED,
    /* 510 */ +NOTEXTENDED,
    /* 511 */ +NETWORKAUTHENTICATIONREQUIRED
](resp: Response[F])

//format: off
object Result {

  /** Result type with completely ambiguous return types */
  type BaseResult[F[_]] = Result[F, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any]

  /** Result with no inferred return types */
  type TopResult[F[_]] = Result[F, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

  /** Existential result type */
  type ExResult[F[_]] = Result[F, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
}
//format: on

trait ResultSyntaxInstances[F[_]] {
  implicit class ResultSyntax[R >: Result.TopResult[F] <: Result.BaseResult[F]](r: R) {
    type Self = R

    private val resp: Response[F] = r.resp

    //def mapK[G[_]](f: ~>[F, G]): R[G] = Result(resp.mapK(f)).asInstanceOf[R[G]]

    def withHttpVersion(httpVersion: HttpVersion): Self = Result(resp.withHttpVersion(httpVersion))

    def withHeaders(headers: Headers): Self = Result(resp.withHeaders(headers))

    def withHeaders(headers: Header*): Self = Result(resp.withHeaders(headers: _*))

    def withAttributes(attributes: Vault): Self = Result(resp.withAttributes(attributes))

    def transformHeaders(f: Headers => Headers): Self = Result(resp.transformHeaders(f))

    def filterHeaders(f: Header => Boolean): Self = Result(resp.filterHeaders(f))

    def removeHeader(key: HeaderKey): Self = Result(resp.removeHeader(key))

    def putHeaders(headers: Header*): Self = Result(resp.putHeaders(headers: _*))

    @scala.deprecated("Use withHeaders instead", "0.20.0-M2")
    def replaceAllHeaders(headers: Headers): Self = Result(resp.replaceAllHeaders(headers))

    @scala.deprecated("Use withHeaders instead", "0.20.0-M2")
    def replaceAllHeaders(headers: Header*): Self = Result(resp.replaceAllHeaders(headers: _*))

    def withTrailerHeaders(trailerHeaders: F[Headers]): Self = Result(
      resp.withTrailerHeaders(trailerHeaders)
    )

    def withoutTrailerHeaders: Self = Result(resp.withoutTrailerHeaders)

    def trailerHeaders(implicit F: Applicative[F]): F[Headers] = resp.trailerHeaders(F)

    @scala.deprecated("Use withContentType(`Content-Type`(t)) instead", "0.20.0-M2")
    def withType(t: MediaType)(implicit F: Functor[F]): Self = Result(resp.withType(t)(F))

    def withContentType(contentType: `Content-Type`): Self = Result(
      resp.withContentType(contentType)
    )

    def withoutContentType: Self = Result(resp.withoutContentType)

    def withContentTypeOption(contentTypeO: Option[`Content-Type`]): Self = Result(
      resp.withContentTypeOption(contentTypeO)
    )

    def contentType: Option[`Content-Type`] = resp.contentType

    def contentLength: Option[Long] = resp.contentLength

    def charset: Option[Charset] = resp.charset

    def isChunked: Boolean = resp.isChunked

    def withAttribute[A](key: Key[A], value: A): Self = Result(resp.withAttribute(key, value))

    def withoutAttribute(key: Key[_]): Self = Result(resp.withoutAttribute(key))

    def attemptAs[T](implicit decoder: EntityDecoder[F, T]): DecodeResult[F, T] =
      resp.attemptAs(decoder)

    def as[T](implicit F: MonadError[F, Throwable], decoder: EntityDecoder[F, T]): F[T] =
      resp.as(F, decoder)

    def withStatus(status: Status): Self = Result(resp.withStatus(status))

    def addCookie(cookie: ResponseCookie): Self = Result(resp.addCookie(cookie))

    def addCookie(name: String, content: String, expires: Option[HttpDate] = None): Self = Result(
      resp.addCookie(name, content, expires)
    )

    def removeCookie(cookie: ResponseCookie): Self = Result(resp.removeCookie(cookie))

    def removeCookie(name: String): Self = Result(resp.removeCookie(name))

    def cookies: List[ResponseCookie] = resp.cookies
  }
}
