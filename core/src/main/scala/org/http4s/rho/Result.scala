package org.http4s
package rho

import cats.{FlatMap, Functor, Monad}

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
  type BaseResult[F[_]] = Result[F, Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]

  /** Result with no inferred return types */
  type TopResult[F[_]]  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]

  /** Existential result type */
  type ExResult[F[_]]   = Result[F, _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]
}

import org.http4s.rho.Result._

trait ResultSyntaxInstances[F[_]] {
  implicit class ResultSyntax[T >: Result.TopResult[F] <: BaseResult[F]](r: T) extends ResponseOps[F] {
    override type Self = T

    override def withStatus(status: Status)(implicit F: Functor[F]): Self =
      Result(r.resp.copy(status = status))

    override def attemptAs[U](implicit F: FlatMap[F], decoder: EntityDecoder[F, U]): DecodeResult[F, U] =
      r.resp.attemptAs(F, decoder)

    override def transformHeaders(f: (Headers) => Headers)(implicit F: Functor[F]): T =
      Result(r.resp.transformHeaders(f))

    override def withAttribute[A](key: AttributeKey[A], value: A)(implicit F: Functor[F]): Self =
      Result(r.resp.withAttribute(key, value))

    def withBody[U](b: U)(implicit F: Monad[F], w: EntityEncoder[F, U]): F[Self] =
      F.pure(Result(r.resp.withEntity(b)))
  }
}
