package org.http4s.rho
package bits

import cats.{Applicative, Monad}
import org.http4s._
import org.http4s.headers.{Location, `Content-Length`}

/** Helpers to aid in the construction of a response function
  *
  * These helpers provide the foundation for the response generation in the rho DSL.
  * They require the needed codecs and generate a response with the correct meta data.
  *
  * {{{
  *   { req => Ok("foo!") }
  * }}}
  */
sealed trait ResponseGenerator {
  def status: Status
}

abstract class EmptyResponseGenerator[F[_]](val status: Status) extends ResponseGenerator {
  type T <: Result.BaseResult[F]
  private def pureResult(implicit F: Applicative[F]): F[Response[F]] = F.pure(Response[F](status))
  private def result(implicit F: Applicative[F]): F[T] =
    F.map(pureResult)(Result(_).asInstanceOf[T])

  /** Generate a [[Result]] that carries the type information */
  def apply(implicit F: Applicative[F]): F[T] = result

  /** Generate wrapper free `Response` */
  def pure(implicit F: Applicative[F]): F[Response[F]] = pureResult
}

abstract class EntityResponseGenerator[F[_]](val status: Status) extends ResponseGenerator {
  type T[_] <: Result.BaseResult[F]

  /** Generate a [[Result]] that carries the type information */
  def apply[A](body: A)(implicit F: Monad[F], w: EntityEncoder[F, A]): F[T[A]] =
    apply(body, Headers.empty)

  /** Generate a [[Result]] that carries the type information */
  def apply[A](body: A, headers: Headers)(implicit F: Monad[F], w: EntityEncoder[F, A]): F[T[A]] = {
    val entity = w.toEntity(body)
    val hs = entity.length match {
      case Some(l) => (w.headers ++ headers).put(`Content-Length`.unsafeFromLong(l))
      case None => w.headers ++ headers
    }
    F.pure(Result(Response[F](status = status, headers = hs, entity = entity)).asInstanceOf[T[A]])
  }

  /** Generate wrapper free `Response` */
  def pure[A](body: A)(implicit F: Monad[F], w: EntityEncoder[F, A]): F[Response[F]] =
    pure(body, Headers.empty)

  /** Generate wrapper free `Response` */
  def pure[A](body: A, headers: Headers)(implicit
      F: Monad[F],
      w: EntityEncoder[F, A]): F[Response[F]] = {
    val entity = w.toEntity(body)
    val hs = entity.length match {
      case Some(l) => (w.headers ++ headers).put(`Content-Length`.unsafeFromLong(l))
      case None => w.headers ++ headers
    }
    F.pure(Response(status = status, headers = hs, entity = entity))
  }

}

abstract class LocationResponseGenerator[F[_]](val status: Status) extends ResponseGenerator {
  type T[_] <: Result.BaseResult[F]

  def apply(location: Uri)(implicit F: Applicative[F]): F[T[Unit]] =
    F.pure(Result(Response(status).putHeaders(Location(location))).asInstanceOf[T[Unit]])

  def apply[A](location: Uri, body: A, headers: Headers = Headers.empty)(implicit
      F: Monad[F],
      w: EntityEncoder[F, A]): F[T[A]] = {
    val entity = w.toEntity(body)
    val hs = (entity.length match {
      case Some(l) => (w.headers ++ headers).put(`Content-Length`.unsafeFromLong(l))
      case None => w.headers ++ headers
    }).put(Location(location))
    F.pure(Result(Response[F](status = status, headers = hs, entity = entity)).asInstanceOf[T[A]])
  }
}

trait ResponseGeneratorInstances[F[_]] {
  //format: off
  // type 1xx                                                 | 100   | 101   | 102   | 103    | 200   | 201   | 202   | 203   | 204   | 205   | 206   | 207   | 208   | 226    | 300   | 301   | 302   | 303   | 304   | 307   | 308    | 400   | 401   | 402   | 403   | 404   | 405   | 406   | 407   | 408   | 409   | 410   | 411   | 412   | 413   | 414   | 415   | 416   | 417   | 421   | 422   | 423   | 424   | 425   | 426   | 428   | 429   | 431   | 451    | 500   | 501   | 502   | 503   | 504   | 505   | 506   | 507   | 508   | 510   | 511  |
  /* 100 */ type CONTINUE                         = Result[F, Unit   ,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 101 */ type SWITCHINGPROTOCOLS               = Result[F, Nothing,Unit   ,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 102 */ type PROCESSING                       = Result[F, Nothing,Nothing,Unit   ,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 103 */ type EARLYHINTS                       = Result[F, Nothing,Nothing,Nothing,Unit   , Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 2xx                                                 | 100   | 101   | 102   | 103    | 200   | 201   | 202   | 203   | 204   | 205   | 206   | 207   | 208   | 226    | 300   | 301   | 302   | 303   | 304   | 307   | 308    | 400   | 401   | 402   | 403   | 404   | 405   | 406   | 407   | 408   | 409   | 410   | 411   | 412   | 413   | 414   | 415   | 416   | 417   | 421   | 422   | 423   | 424   | 425   | 426   | 428   | 429   | 431   | 451    | 500   | 501   | 502   | 503   | 504   | 505   | 506   | 507   | 508   | 510   | 511  |
  /* 200 */ type OK[A]                            = Result[F, Nothing,Nothing,Nothing,Nothing, A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 201 */ type CREATED[A]                       = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 202 */ type ACCEPTED[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 203 */ type NONAUTHORITATIVEINFORMATION[A]   = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 204 */ type NOCONTENT                        = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 205 */ type RESETCONTENT                     = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 206 */ type PARTIALCONTENT[A]                = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 207 */ type MULTISTATUS[A]                   = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 208 */ type ALREADYREPORTED[A]               = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 226 */ type IMUSED[A]                        = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      , Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 3xx                                                 | 100   | 101   | 102   | 103    | 200   | 201   | 202   | 203   | 204   | 205   | 206   | 207   | 208   | 226    | 300   | 301   | 302   | 303   | 304   | 307   | 308    | 400   | 401   | 402   | 403   | 404   | 405   | 406   | 407   | 408   | 409   | 410   | 411   | 412   | 413   | 414   | 415   | 416   | 417   | 421   | 422   | 423   | 424   | 425   | 426   | 428   | 429   | 431   | 451    | 500   | 501   | 502   | 503   | 504   | 505   | 506   | 507   | 508   | 510   | 511  |
  /* 300 */ type MULTIPLECHOICES[A]               = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 301 */ type MOVEDPERMANENTLY[A]              = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 302 */ type FOUND[A]                         = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 303 */ type SEEOTHER[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 304 */ type NOTMODIFIED                      = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 307 */ type TEMPORARYREDIRECT[A]             = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 308 */ type PERMANENTREDIRECT[A]             = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      , Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 4xx                                                 | 100   | 101   | 102   | 103    | 200   | 201   | 202   | 203   | 204   | 205   | 206   | 207   | 208   | 226    | 300   | 301   | 302   | 303   | 304   | 307   | 308    | 400   | 401   | 402   | 403   | 404   | 405   | 406   | 407   | 408   | 409   | 410   | 411   | 412   | 413   | 414   | 415   | 416   | 417   | 421   | 422   | 423   | 424   | 425   | 426   | 428   | 429   | 431   | 451    | 500   | 501   | 502   | 503   | 504   | 505   | 506   | 507   | 508   | 510   | 511  |
  /* 400 */ type BADREQUEST[A]                    = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 401 */ type UNAUTHORIZED[A]                  = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 402 */ type PAYMENTREQUIRED[A]               = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 403 */ type FORBIDDEN[A]                     = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 404 */ type NOTFOUND[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 405 */ type METHODNOTALLOWED[A]              = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 406 */ type NOTACCEPTABLE[A]                 = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 407 */ type PROXYAUTHENTICATIONREQUIRED[A]   = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 408 */ type REQUESTTIMEOUT[A]                = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 409 */ type CONFLICT[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 410 */ type GONE[A]                          = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 411 */ type LENGTHREQUIRED[A]                = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 412 */ type PRECONDITIONFAILED[A]            = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 413 */ type PAYLOADTOOLARGE[A]               = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 414 */ type URITOOLONG[A]                    = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 415 */ type UNSUPPORTEDMEDIATYPE[A]          = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 416 */ type RANGENOTSATISFIABLE[A]           = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 417 */ type EXPECTATIONFAILED[A]             = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 421 */ type MISDIRECTEDREQUEST[A]            = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 422 */ type UNPROCESSABLEENTITY[A]           = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 423 */ type LOCKED[A]                        = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 424 */ type FAILEDDEPENDENCY[A]              = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 425 */ type TOOEARLY[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 426 */ type UPGRADEREQUIRED[A]               = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 428 */ type PRECONDITIONREQUIRED[A]          = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 429 */ type TOOMANYREQUESTS[A]               = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 431 */ type REQUESTHEADERFIELDSTOOLARGE[A]   = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 451 */ type UNAVAILABLEFORLEGALREASONS[A]    = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      , Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 5xx                                                 | 100   | 101   | 102   | 103    | 200   | 201   | 202   | 203   | 204   | 205   | 206   | 207   | 208   | 226    | 300   | 301   | 302   | 303   | 304   | 307   | 308    | 400   | 401   | 402   | 403   | 404   | 405   | 406   | 407   | 408   | 409   | 410   | 411   | 412   | 413   | 414   | 415   | 416   | 417   | 421   | 422   | 423   | 424   | 425   | 426   | 428   | 429   | 431   | 451    | 500   | 501   | 502   | 503   | 504   | 505   | 506   | 507   | 508   | 510   | 511  |
  /* 500 */ type INTERNALSERVERERROR[A]           = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 501 */ type NOTIMPLEMENTED[A]                = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 502 */ type BADGATEWAY[A]                    = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 503 */ type SERVICEUNAVAILABLE[A]            = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 504 */ type GATEWAYTIMEOUT[A]                = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 505 */ type HTTPVERSIONNOTSUPPORTED[A]       = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing,Nothing]
  /* 506 */ type VARIANTALSONEGOTIATES[A]         = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing,Nothing]
  /* 507 */ type INSUFFICIENTSTORAGE[A]           = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing,Nothing]
  /* 508 */ type LOOPDETECTED[A]                  = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing,Nothing]
  /* 510 */ type NOTEXTENDED[A]                   = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ,Nothing]
  /* 511 */ type NETWORKAUTHENTICATIONREQUIRED[A] = Result[F, Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,A      ]
  //format: on

  // 1xx
  /* 100 */
  object Continue extends EmptyResponseGenerator[F](Status.Continue) {
    type T = CONTINUE
  }

  /* 101 */
  object SwitchingProtocols extends EmptyResponseGenerator[F](Status.SwitchingProtocols) {
    type T = SWITCHINGPROTOCOLS
  }

  /* 102 */
  object Processing extends EmptyResponseGenerator[F](Status.Processing) {
    type T = PROCESSING
  }

  /* 103 */
  object EarlyHints extends EmptyResponseGenerator[F](Status.EarlyHints) {
    type T = EARLYHINTS
  }

  // 2xx
  /* 200 */
  object Ok extends EntityResponseGenerator[F](Status.Ok) {
    type T[A] = OK[A]
  }

  /* 201 */
  object Created extends EntityResponseGenerator[F](Status.Created) {
    type T[A] = CREATED[A]
  }

  /* 202 */
  object Accepted extends EntityResponseGenerator[F](Status.Accepted) {
    type T[A] = ACCEPTED[A]
  }

  /* 203 */
  object NonAuthoritativeInformation
      extends EntityResponseGenerator[F](Status.NonAuthoritativeInformation) {
    type T[A] = NONAUTHORITATIVEINFORMATION[A]
  }

  /* 204 */
  object NoContent extends EmptyResponseGenerator[F](Status.NoContent) {
    type T = NOCONTENT
  }

  /* 205 */
  object ResetContent extends EmptyResponseGenerator[F](Status.ResetContent) {
    type T = RESETCONTENT
  }

  /* 206 */
  object PartialContent extends EntityResponseGenerator[F](Status.PartialContent) {
    type T[A] = PARTIALCONTENT[A]
  }

  /* 207 */
  object MultiStatus extends EntityResponseGenerator[F](Status.MultiStatus) {
    type T[A] = MULTISTATUS[A]
  }

  /* 208 */
  object AlreadyReported extends EntityResponseGenerator[F](Status.AlreadyReported) {
    type T[A] = ALREADYREPORTED[A]
  }

  /* 226 */
  object IMUsed extends EntityResponseGenerator[F](Status.IMUsed) {
    type T[A] = IMUSED[A]
  }

  // 3xx
  /* 300 */
  object MultipleChoices extends LocationResponseGenerator[F](Status.MultipleChoices) {
    type T[A] = MULTIPLECHOICES[A]
  }

  /* 301 */
  object MovedPermanently extends LocationResponseGenerator[F](Status.MovedPermanently) {
    type T[A] = MOVEDPERMANENTLY[A]
  }

  /* 302 */
  object Found extends LocationResponseGenerator[F](Status.Found) {
    type T[A] = FOUND[A]
  }

  /* 303 */
  object SeeOther extends LocationResponseGenerator[F](Status.SeeOther) {
    type T[A] = SEEOTHER[A]
  }

  /* 304 */
  object NotModified extends EmptyResponseGenerator[F](Status.NotModified) {
    type T = NOTMODIFIED
  }

  // UseProxy has been deprecated in RFC7231 so it is omitted.

  /* 307 */
  object TemporaryRedirect extends LocationResponseGenerator[F](Status.TemporaryRedirect) {
    type T[A] = TEMPORARYREDIRECT[A]
  }

  /* 308 */
  object PermanentRedirect extends LocationResponseGenerator[F](Status.PermanentRedirect) {
    type T[A] = PERMANENTREDIRECT[A]
  }

  // 4xx
  /* 400 */
  object BadRequest extends EntityResponseGenerator[F](Status.BadRequest) {
    type T[A] = BADREQUEST[A]
  }

  /* 401 */
  object Unauthorized extends EntityResponseGenerator[F](Status.Unauthorized) {
    type T[A] = UNAUTHORIZED[A]
  }

  /* 402 */
  object PaymentRequired extends EntityResponseGenerator[F](Status.PaymentRequired) {
    type T[A] = PAYMENTREQUIRED[A]
  }

  /* 403 */
  object Forbidden extends EntityResponseGenerator[F](Status.Forbidden) {
    type T[A] = FORBIDDEN[A]
  }

  /* 404 */
  object NotFound extends EntityResponseGenerator[F](Status.NotFound) {
    type T[A] = NOTFOUND[A]
  }

  /* 405 */
  object MethodNotAllowed extends EntityResponseGenerator[F](Status.MethodNotAllowed) {
    type T[A] = METHODNOTALLOWED[A]
  }

  /* 406 */
  object NotAcceptable extends EntityResponseGenerator[F](Status.NotAcceptable) {
    type T[A] = NOTACCEPTABLE[A]
  }

  /* 407 */
  object ProxyAuthenticationRequired
      extends EntityResponseGenerator[F](Status.ProxyAuthenticationRequired) {
    type T[A] = PROXYAUTHENTICATIONREQUIRED[A]
  }

  /* 408 */
  object RequestTimeout extends EntityResponseGenerator[F](Status.RequestTimeout) {
    type T[A] = REQUESTTIMEOUT[A]
  }

  /* 409 */
  object Conflict extends EntityResponseGenerator[F](Status.Conflict) {
    type T[A] = CONFLICT[A]
  }

  /* 410 */
  object Gone extends EntityResponseGenerator[F](Status.Gone) {
    type T[A] = GONE[A]
  }

  /* 411 */
  object LengthRequired extends EntityResponseGenerator[F](Status.LengthRequired) {
    type T[A] = LENGTHREQUIRED[A]
  }

  /* 412 */
  object PreconditionFailed extends EntityResponseGenerator[F](Status.PreconditionFailed) {
    type T[A] = PRECONDITIONFAILED[A]
  }

  /* 413 */
  object PayloadTooLarge extends EntityResponseGenerator[F](Status.PayloadTooLarge) {
    type T[A] = PAYLOADTOOLARGE[A]
  }

  /* 414 */
  object UriTooLong extends EntityResponseGenerator[F](Status.UriTooLong) {
    type T[A] = URITOOLONG[A]
  }

  /* 415 */
  object UnsupportedMediaType extends EntityResponseGenerator[F](Status.UnsupportedMediaType) {
    type T[A] = UNSUPPORTEDMEDIATYPE[A]
  }

  /* 416 */
  object RangeNotSatisfiable extends EntityResponseGenerator[F](Status.RangeNotSatisfiable) {
    type T[A] = RANGENOTSATISFIABLE[A]
  }

  /* 417 */
  object ExpectationFailed extends EntityResponseGenerator[F](Status.ExpectationFailed) {
    type T[A] = EXPECTATIONFAILED[A]
  }

  /* 421 */
  object MisdirectedRequest extends EntityResponseGenerator[F](Status.MisdirectedRequest) {
    type T[A] = MISDIRECTEDREQUEST[A]
  }

  /* 422 */
  object UnprocessableEntity extends EntityResponseGenerator[F](Status.UnprocessableEntity) {
    type T[A] = UNPROCESSABLEENTITY[A]
  }

  /* 423 */
  object Locked extends EntityResponseGenerator[F](Status.Locked) {
    type T[A] = LOCKED[A]
  }

  /* 424 */
  object FailedDependency extends EntityResponseGenerator[F](Status.FailedDependency) {
    type T[A] = FAILEDDEPENDENCY[A]
  }

  /* 425 */
  object TooEarly extends EntityResponseGenerator[F](Status.TooEarly) {
    type T[A] = TOOEARLY[A]
  }

  /* 426 */
  object UpgradeRequired extends EntityResponseGenerator[F](Status.UpgradeRequired) {
    type T[A] = UPGRADEREQUIRED[A]
  }

  /* 428 */
  object PreconditionRequired extends EntityResponseGenerator[F](Status.PreconditionRequired) {
    type T[A] = PRECONDITIONREQUIRED[A]
  }

  /* 429 */
  object TooManyRequests extends EntityResponseGenerator[F](Status.TooManyRequests) {
    type T[A] = TOOMANYREQUESTS[A]
  }

  /* 431 */
  object RequestHeaderFieldsTooLarge
      extends EntityResponseGenerator[F](Status.RequestHeaderFieldsTooLarge) {
    type T[A] = REQUESTHEADERFIELDSTOOLARGE[A]
  }

  /* 451 */
  object UnavailableForLegalReasons
      extends EntityResponseGenerator[F](Status.UnavailableForLegalReasons) {
    type T[A] = UNAVAILABLEFORLEGALREASONS[A]
  }

  // 5xx
  /* 500 */
  object InternalServerError extends EntityResponseGenerator[F](Status.InternalServerError) {
    type T[A] = INTERNALSERVERERROR[A]
  }

  /* 501 */
  object NotImplemented extends EntityResponseGenerator[F](Status.NotImplemented) {
    type T[A] = NOTIMPLEMENTED[A]
  }

  /* 502 */
  object BadGateway extends EntityResponseGenerator[F](Status.BadGateway) {
    type T[A] = BADGATEWAY[A]
  }

  /* 503 */
  object ServiceUnavailable extends EntityResponseGenerator[F](Status.ServiceUnavailable) {
    type T[A] = SERVICEUNAVAILABLE[A]
  }

  /* 504 */
  object GatewayTimeout extends EntityResponseGenerator[F](Status.GatewayTimeout) {
    type T[A] = GATEWAYTIMEOUT[A]
  }

  /* 505 */
  object HttpVersionNotSupported
      extends EntityResponseGenerator[F](Status.HttpVersionNotSupported) {
    type T[A] = HTTPVERSIONNOTSUPPORTED[A]
  }

  /* 506 */
  object VariantAlsoNegotiates extends EntityResponseGenerator[F](Status.VariantAlsoNegotiates) {
    type T[A] = VARIANTALSONEGOTIATES[A]
  }

  /* 507 */
  object InsufficientStorage extends EntityResponseGenerator[F](Status.InsufficientStorage) {
    type T[A] = INSUFFICIENTSTORAGE[A]
  }

  /* 508 */
  object LoopDetected extends EntityResponseGenerator[F](Status.LoopDetected) {
    type T[A] = LOOPDETECTED[A]
  }

  /* 510 */
  object NotExtended extends EntityResponseGenerator[F](Status.NotExtended) {
    type T[A] = NOTEXTENDED[A]
  }

  /* 511 */
  object NetworkAuthenticationRequired
      extends EntityResponseGenerator[F](Status.NetworkAuthenticationRequired) {
    type T[A] = NETWORKAUTHENTICATIONREQUIRED[A]
  }
}
