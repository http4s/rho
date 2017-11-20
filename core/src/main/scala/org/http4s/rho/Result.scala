package org.http4s
package rho

import cats.{FlatMap, Functor, Monad}
import cats.data.EitherT
import cats.effect.IO

/** A helper for capturing the result types and status codes from routes */
sealed case class Result[
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
](resp: Response[IO])

object Result {

  /** Result type with completely ambiguous return types */
  type BaseResult = Result[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]

  /** Result with no inferred return types */
  type TopResult  = Result[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]

  /** Existential result type */
  type ExResult   = Result[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]
}

import Result._

trait ResultSyntaxInstances {

  implicit class ResultSyntax[T >: Result.TopResult <: BaseResult](r: T) extends ResponseOps[IO] {
    override type Self = T

    override def withStatus(status: Status)(implicit F: Functor[IO]): Self =
      Result(r.resp.copy(status = status))


    override def attemptAs[T](implicit F: FlatMap[IO], decoder: EntityDecoder[IO, T]): DecodeResult[IO, T] = {
      val t: IO[Either[DecodeFailure, T]] = r.resp.attemptAs(FlatMap[IO], decoder).value
      EitherT[IO, DecodeFailure, T](t)
    }

    override def transformHeaders(f: (Headers) => Headers)(implicit F: Functor[IO]): T =
      Result(r.resp.transformHeaders(f))

    override def withAttribute[A](key: AttributeKey[A], value: A)(implicit F: Functor[IO]): Self =
      Result(r.resp.withAttribute(key, value))

    def withBody[T](b: T)(implicit w: EntityEncoder[IO, T]): IO[Self] = {
      r.resp.withBody(b)(Monad[IO], w).map(Result(_))
    }
  }

  implicit class IOResultSyntax[T >: Result.TopResult <: BaseResult](r: IO[T]) extends ResponseOps[IO] {
    override type Self = IO[T]

    override def withStatus(status: Status)(implicit F: Functor[IO]): Self = r.map{ result =>
      Result(result.resp.copy(status = status))
    }

    override def attemptAs[T](implicit F: FlatMap[IO], decoder: EntityDecoder[IO, T]): DecodeResult[IO, T] = {
      val t: IO[Either[DecodeFailure, T]] = r.flatMap { t =>
        t.resp.attemptAs(FlatMap[IO], decoder).value
      }
      EitherT[IO, DecodeFailure, T](t)
    }

    override def withAttribute[A](key: AttributeKey[A], value: A)(implicit F: Functor[IO]): Self =
      r.map(r => Result(r.resp.withAttribute(key, value)))


    override def transformHeaders(f: (Headers) => Headers)(implicit F: Functor[IO]): IO[T] = r.map { result =>
      Result(result.resp.transformHeaders(f))
    }

    def withBody[T](b: T)(implicit w: EntityEncoder[IO, T]): Self = {
      r.flatMap(_.withBody(b)(w))
    }
  }
}
