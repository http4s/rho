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
  private def result(implicit F: Applicative[F]): F[T] = F.map(pureResult)(Result(_).asInstanceOf[T])

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
    F.flatMap(w.toEntity(body)) { case Entity(proc, len) =>
      val hs = len match {
        case Some(l) => (w.headers ++ headers).put(`Content-Length`.unsafeFromLong(l))
        case None    => w.headers ++ headers
      }
      F.pure(Result(Response[F](status = status, headers = hs, body = proc)).asInstanceOf[T[A]])
    }
  }

  /** Generate wrapper free `Response` */
  def pure[A](body: A)(implicit F: Monad[F], w: EntityEncoder[F, A]): F[Response[F]] =
    pure(body, Headers.empty)

  /** Generate wrapper free `Response` */
  def pure[A](body: A, headers: Headers)(implicit F: Monad[F], w: EntityEncoder[F, A]): F[Response[F]] = {
    F.flatMap(w.toEntity(body)) { case Entity(proc, len) =>
      val hs = len match {
        case Some(l) => (w.headers ++ headers).put(`Content-Length`.unsafeFromLong(l))
        case None    => w.headers ++ headers
      }
      F.pure(Response(status = status, headers = hs, body = proc))
    }
  }

}

abstract class LocationResponseGenerator[F[_]](val status: Status) extends ResponseGenerator {
  type T <: Result.BaseResult[F]

  def apply(location: Uri)(implicit F: Applicative[F]): F[T] =
    F.pure(Result(Response(status).putHeaders(Location(location))).asInstanceOf[T])
}

trait ResponseGeneratorInstances[F[_]] {
  //                                                      1       2       3       4       5       6       7       8       9       10      11      12      13      14      15      16      17      18      19      20      21      22      23      24      25      26      27      28      29      30      31      32      33       34      35      36      37      38      39      40      41      42      43      44      45      46      47      48      49      50      51      52      53      54      55      56      57
  // type 1xx
  type CONTINUE                         = Result[F, Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type SWITCHINGPROTOCOLS               = Result[F, Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PROCESSING                       = Result[F, Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 2xx
  type OK[A]                            = Result[F, Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type CREATED[A]                       = Result[F, Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type ACCEPTED[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NONAUTHORITATIVEINFORMATION[A]   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOCONTENT                        = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type RESETCONTENT                     = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PARTIALCONTENT[A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type MULTISTATUS[A]                   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type ALREADYREPORTED[A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type IMUSED[A]                        = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 3xx
  type MULTIPLECHOICES                  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type MOVEDPERMANENTLY                 = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type FOUND                            = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type SEEOTHER                         = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTMODIFIED                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type TEMPORARYREDIRECT                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PERMANENTREDIRECT                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Unit   ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 4xx
  //                                               1       2       3       4       5       6       7       8       9       10      11      12      13      14      15      16      17      18      19      20      21      22      23      24      25      26      27      28      29      30      31      32      33       34      35      36      37      38      39      40      41      42      43      44      45      46      47      48      49      50      51      52      53      54      55      56      57
  type BADREQUEST[A]                    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNAUTHORIZED[A]                  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PAYMENTREQUIRED[A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type FORBIDDEN[A]                     = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTFOUND[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type METHODNOTALLOWED[A]              = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTACCEPTABLE[A]                 = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PROXYAUTHENTICATIONREQUIRED[A]   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type REQUESTTIMEOUT[A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type CONFLICT[A]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type GONE[A]                          = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type LENGTHREQUIRED[A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A     ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PRECONDITIONFAILED[A]            = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PAYLOADTOOLARGE[A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type URITOOLONG[A]                    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNSUPPORTEDMEDIATYPE[A]          = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type RANGENOTSATISFIABLE[A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type EXPECTATIONFAILED[A]             = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNPROCESSABLEENTITY[A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type LOCKED[A]                        = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type FAILEDDEPENDENCY[A]              = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UPGRADEREQUIRED[A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PRECONDITIONREQUIRED[A]          = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type TOOMANYREQUESTS[A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type REQUESTHEADERFIELDSTOOLARGE[A]   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNAVAILABLEFORLEGALREASONS[A]    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 5xx
  type INTERNALSERVERERROR[A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTIMPLEMENTED[A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type BADGATEWAY[A]                    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type SERVICEUNAVAILABLE[A]            = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type GATEWAYTIMEOUT[A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type HTTPVERSIONNOTSUPPORTED[A]       = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type VARIANTALSONEGOTIATES[A]         = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing]
  type INSUFFICIENTSTORAGE[A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing]
  type LOOPDETECTED[A]                  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing]
  type NOTEXTENDED[A]                   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing]
  type NETWORKAUTHENTICATIONREQUIRED[A] = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ]


  // 1xx
  object Continue extends EmptyResponseGenerator[F](Status.Continue) {
    type T = CONTINUE
  }

  object SwitchingProtocols extends EmptyResponseGenerator[F](Status.SwitchingProtocols) {
    type T = SWITCHINGPROTOCOLS
  }

  object Processing extends EmptyResponseGenerator[F](Status.Processing) {
    type T = PROCESSING
  }

  // 2xx
  object Ok extends EntityResponseGenerator[F](Status.Ok) {
    type T[A] = OK[A]
  }

  object Created extends EntityResponseGenerator[F](Status.Created) {
    type T[A] = CREATED[A]
  }

  object Accepted extends EntityResponseGenerator[F](Status.Accepted) {
    type T[A] = ACCEPTED[A]
  }

  object NonAuthoritativeInformation extends EntityResponseGenerator[F](Status.NonAuthoritativeInformation) {
    type T[A] = NONAUTHORITATIVEINFORMATION[A]
  }

  object NoContent extends EmptyResponseGenerator[F](Status.NoContent) {
    type T = NOCONTENT
  }

  object ResetContent extends EmptyResponseGenerator[F](Status.ResetContent) {
    type T = RESETCONTENT
  }

  object PartialContent extends EntityResponseGenerator[F](Status.PartialContent) {
    type T[A] = PARTIALCONTENT[A]
  }

  object MultiStatus extends EntityResponseGenerator[F](Status.MultiStatus) {
    type T[A] = MULTISTATUS[A]
  }

  object AlreadyReported extends EntityResponseGenerator[F](Status.AlreadyReported) {
    type T[A] = ALREADYREPORTED[A]
  }

  object IMUsed extends EntityResponseGenerator[F](Status.IMUsed) {
    type T[A] = IMUSED[A]
  }

  // 3xx
  object MultipleChoices extends LocationResponseGenerator[F](Status.MultipleChoices) {
    type T = MULTIPLECHOICES
  }

  object MovedPermanently extends LocationResponseGenerator[F](Status.MovedPermanently) {
    type T = MOVEDPERMANENTLY
  }

  object Found extends LocationResponseGenerator[F](Status.Found) {
    type T = FOUND
  }

  object SeeOther extends LocationResponseGenerator[F](Status.SeeOther) {
    type T = SEEOTHER
  }

  object NotModified extends EmptyResponseGenerator[F](Status.NotModified) {
    type T = NOTMODIFIED
  }

  // UseProxy has been deprecated in RFC7231 so it is omitted.

  object TemporaryRedirect extends LocationResponseGenerator[F](Status.TemporaryRedirect) {
    type T = TEMPORARYREDIRECT
  }

  object PermanentRedirect extends LocationResponseGenerator[F](Status.PermanentRedirect) {
    type T = PERMANENTREDIRECT
  }

  // 4xx
  object BadRequest extends EntityResponseGenerator[F](Status.BadRequest) {
    type T[A] = BADREQUEST[A]
  }

  object Unauthorized extends EntityResponseGenerator[F](Status.Unauthorized) {
    type T[A] = UNAUTHORIZED[A]
  }

  object PaymentRequired extends EntityResponseGenerator[F](Status.PaymentRequired) {
    type T[A] = PAYMENTREQUIRED[A]
  }

  object Forbidden extends EntityResponseGenerator[F](Status.Forbidden) {
    type T[A] = FORBIDDEN[A]
  }

  object NotFound extends EntityResponseGenerator[F](Status.NotFound) {
    type T[A] = NOTFOUND[A]
  }

  object MethodNotAllowed extends EntityResponseGenerator[F](Status.MethodNotAllowed) {
    type T[A] = METHODNOTALLOWED[A]
  }

  object NotAcceptable extends EntityResponseGenerator[F](Status.NotAcceptable) {
    type T[A] = NOTACCEPTABLE[A]
  }

  object ProxyAuthenticationRequired extends EntityResponseGenerator[F](Status.ProxyAuthenticationRequired) {
    type T[A] = PROXYAUTHENTICATIONREQUIRED[A]
  }

  object RequestTimeout extends EntityResponseGenerator[F](Status.RequestTimeout) {
    type T[A] = REQUESTTIMEOUT[A]
  }

  object Conflict extends EntityResponseGenerator[F](Status.Conflict) {
    type T[A] = CONFLICT[A]
  }

  object Gone extends EntityResponseGenerator[F](Status.Gone) {
    type T[A] = GONE[A]
  }

  object LengthRequired extends EntityResponseGenerator[F](Status.LengthRequired) {
    type T[A] = LENGTHREQUIRED[A]
  }

  object PreconditionFailed extends EntityResponseGenerator[F](Status.PreconditionFailed) {
    type T[A] = PRECONDITIONFAILED[A]
  }

  object PayloadTooLarge extends EntityResponseGenerator[F](Status.PayloadTooLarge) {
    type T[A] = PAYLOADTOOLARGE[A]
  }

  object UriTooLong extends EntityResponseGenerator[F](Status.UriTooLong) {
    type T[A] = URITOOLONG[A]
  }

  object UnsupportedMediaType extends EntityResponseGenerator[F](Status.UnsupportedMediaType) {
    type T[A] = UNSUPPORTEDMEDIATYPE[A]
  }

  object RangeNotSatisfiable extends EntityResponseGenerator[F](Status.RangeNotSatisfiable) {
    type T[A] = RANGENOTSATISFIABLE[A]
  }

  object ExpectationFailed extends EntityResponseGenerator[F](Status.ExpectationFailed) {
    type T[A] = EXPECTATIONFAILED[A]
  }

  object UnprocessableEntity extends EntityResponseGenerator[F](Status.UnprocessableEntity) {
    type T[A] = UNPROCESSABLEENTITY[A]
  }

  object Locked extends EntityResponseGenerator[F](Status.Locked) {
    type T[A] = LOCKED[A]
  }

  object FailedDependency extends EntityResponseGenerator[F](Status.FailedDependency) {
    type T[A] = FAILEDDEPENDENCY[A]
  }

  object UpgradeRequired extends EntityResponseGenerator[F](Status.UpgradeRequired) {
    type T[A] = UPGRADEREQUIRED[A]
  }

  object PreconditionRequired extends EntityResponseGenerator[F](Status.PreconditionRequired) {
    type T[A] = PRECONDITIONREQUIRED[A]
  }

  object TooManyRequests extends EntityResponseGenerator[F](Status.TooManyRequests) {
    type T[A] = TOOMANYREQUESTS[A]
  }

  object RequestHeaderFieldsTooLarge extends EntityResponseGenerator[F](Status.RequestHeaderFieldsTooLarge) {
    type T[A] = REQUESTHEADERFIELDSTOOLARGE[A]
  }

  object UnavailableForLegalReasons extends EntityResponseGenerator[F](Status.UnavailableForLegalReasons) {
    type T[A] = UNAVAILABLEFORLEGALREASONS[A]
  }

  // 5xx
  object InternalServerError extends EntityResponseGenerator[F](Status.InternalServerError) {
    type T[A] = INTERNALSERVERERROR[A]
  }

  object NotImplemented extends EntityResponseGenerator[F](Status.NotImplemented) {
    type T[A] = NOTIMPLEMENTED[A]
  }

  object BadGateway extends EntityResponseGenerator[F](Status.BadGateway) {
    type T[A] = BADGATEWAY[A]
  }

  object ServiceUnavailable extends EntityResponseGenerator[F](Status.ServiceUnavailable) {
    type T[A] = SERVICEUNAVAILABLE[A]
  }

  object GatewayTimeout extends EntityResponseGenerator[F](Status.GatewayTimeout) {
    type T[A] = GATEWAYTIMEOUT[A]
  }

  object HttpVersionNotSupported extends EntityResponseGenerator[F](Status.HttpVersionNotSupported) {
    type T[A] = HTTPVERSIONNOTSUPPORTED[A]
  }

  object VariantAlsoNegotiates extends EntityResponseGenerator[F](Status.VariantAlsoNegotiates) {
    type T[A] = VARIANTALSONEGOTIATES[A]
  }

  object InsufficientStorage extends EntityResponseGenerator[F](Status.InsufficientStorage) {
    type T[A] = INSUFFICIENTSTORAGE[A]
  }

  object LoopDetected extends EntityResponseGenerator[F](Status.LoopDetected) {
    type T[A] = LOOPDETECTED[A]
  }

  object NotExtended extends EntityResponseGenerator[F](Status.NotExtended) {
    type T[A] = NOTEXTENDED[A]
  }

  object NetworkAuthenticationRequired extends EntityResponseGenerator[F](Status.NetworkAuthenticationRequired) {
    type T[A] = NETWORKAUTHENTICATIONREQUIRED[A]
  }
}
