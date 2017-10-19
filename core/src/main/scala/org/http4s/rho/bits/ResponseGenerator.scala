package org.http4s.rho
package bits

import cats.{Applicative, Monad}
import org.http4s._
import org.http4s.headers.{Location, `Content-Length`}
import org.http4s.rho.bits.ResponseGenerator.EmptyRe

// TODO: ????
import fs2.Chunk

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

object ResponseGenerator {
  case class EmptyRe()

  object EmptyRe {
    // This is just a dummy so that the implicits in ResultMatcher will work.
    implicit def w[F[_]](implicit F: Applicative[F]): EntityEncoder[F, EmptyRe] = {
      EntityEncoder.simple[F, EmptyRe]()(_ => Chunk.empty)
    }
  }
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

object ResponseGeneratorInstances extends ResponseGeneratorInstances

trait ResponseGeneratorInstances {
  //                                                      1       2       3       4       5       6       7       8       9       10      11      12      13      14      15      16      17      18      19      20      21      22      23      24      25      26      27      28      29      30      31      32      33       34      35      36      37      38      39      40      41      42      43      44      45      46      47      48      49      50      51      52      53      54      55      56      57
  // type 1xx
  type CONTINUE[F[_]]                         = Result[F, EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type SWITCHINGPROTOCOLS[F[_]]               = Result[F, Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PROCESSING[F[_]]                       = Result[F, Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 2xx
  type OK[F[_], A]                            = Result[F, Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type CREATED[F[_], A]                       = Result[F, Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type ACCEPTED[F[_], A]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NONAUTHORITATIVEINFORMATION[F[_], A]   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOCONTENT[F[_]]                        = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type RESETCONTENT[F[_]]                     = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PARTIALCONTENT[F[_], A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type MULTISTATUS[F[_], A]                   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type ALREADYREPORTED[F[_], A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type IMUSED[F[_], A]                        = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 3xx
  type MULTIPLECHOICES[F[_]]                  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type MOVEDPERMANENTLY[F[_]]                 = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type FOUND[F[_]]                            = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type SEEOTHER[F[_]]                         = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTMODIFIED[F[_]]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type TEMPORARYREDIRECT[F[_]]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PERMANENTREDIRECT[F[_]]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,EmptyRe,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 4xx
  //                                               1       2       3       4       5       6       7       8       9       10      11      12      13      14      15      16      17      18      19      20      21      22      23      24      25      26      27      28      29      30      31      32      33       34      35      36      37      38      39      40      41      42      43      44      45      46      47      48      49      50      51      52      53      54      55      56      57
  type BADREQUEST[F[_], A]                    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNAUTHORIZED[F[_], A]                  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PAYMENTREQUIRED[F[_], A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type FORBIDDEN[F[_], A]                     = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTFOUND[F[_], A]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type METHODNOTALLOWED[F[_], A]              = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTACCEPTABLE[F[_], A]                 = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PROXYAUTHENTICATIONREQUIRED[F[_], A]   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type REQUESTTIMEOUT[F[_], A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type CONFLICT[F[_], A]                      = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type GONE[F[_], A]                          = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type LENGTHREQUIRED[F[_], A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A     ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PRECONDITIONFAILED[F[_], A]            = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PAYLOADTOOLARGE[F[_], A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type URITOOLONG[F[_], A]                    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNSUPPORTEDMEDIATYPE[F[_], A]          = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type RANGENOTSATISFIABLE[F[_], A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type EXPECTATIONFAILED[F[_], A]             = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UNPROCESSABLEENTITY[F[_], A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type LOCKED[F[_], A]                        = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type FAILEDDEPENDENCY[F[_], A]              = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type UPGRADEREQUIRED[F[_], A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type PRECONDITIONREQUIRED[F[_], A]          = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type TOOMANYREQUESTS[F[_], A]               = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type REQUESTHEADERFIELDSTOOLARGE[F[_], A]   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  // type 5xx
  type INTERNALSERVERERROR[F[_], A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type NOTIMPLEMENTED[F[_], A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type BADGATEWAY[F[_], A]                    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type SERVICEUNAVAILABLE[F[_], A]            = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type GATEWAYTIMEOUT[F[_], A]                = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type HTTPVERSIONNOTSUPPORTED[F[_], A]       = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type VARIANTALSONEGOTIATES[F[_], A]         = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing]
  type INSUFFICIENTSTORAGE[F[_], A]           = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing]
  type LOOPDETECTED[F[_], A]                  = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing]
  type NOTEXTENDED[F[_], A]                   = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing]
  type NETWORKAUTHENTICATIONREQUIRED[F[_], A] = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ]


  object Continue                      extends EmptyResponseGenerator(Status.Continue) { type T[F[_]]  = CONTINUE[F] }
  object SwitchingProtocols            extends EmptyResponseGenerator(Status.SwitchingProtocols) { type T[F[_]] = SWITCHINGPROTOCOLS[F] }
  object Processing                    extends EmptyResponseGenerator(Status.Processing) { type T[F[_]] = PROCESSING[F] }

  object Ok                            extends EntityResponseGenerator(Status.Ok) { type T[F[_], A] = OK[F, A] }
  object Created                       extends EntityResponseGenerator(Status.Created) { type T[F[_], A] = CREATED[F, A] }
  object Accepted                      extends EntityResponseGenerator(Status.Accepted) { type T[F[_], A] = ACCEPTED[F, A] }
  object NonAuthoritativeInformation   extends EntityResponseGenerator(Status.NonAuthoritativeInformation) { type T[F[_], A] = NONAUTHORITATIVEINFORMATION[F, A] }
  object NoContent                     extends EmptyResponseGenerator(Status.NoContent) { type T[F[_]] = NOCONTENT[F] }
  object ResetContent                  extends EmptyResponseGenerator(Status.ResetContent) { type T[F[_]] = RESETCONTENT[F] }
  object PartialContent                extends EntityResponseGenerator(Status.PartialContent) { type T[F[_], A] = PARTIALCONTENT[F, A] }
  object MultiStatus                   extends EntityResponseGenerator(Status.MultiStatus) { type T[F[_], A] = MULTISTATUS[F, A] }
  object AlreadyReported               extends EntityResponseGenerator(Status.AlreadyReported) { type T[F[_], A] = ALREADYREPORTED[F, A] }
  object IMUsed                        extends EntityResponseGenerator(Status.IMUsed) { type T[F[_], A] = IMUSED[F, A] }

  object MultipleChoices               extends LocationResponseGenerator(Status.MultipleChoices) { type T[F[_]] = MULTIPLECHOICES[F] }
  object MovedPermanently              extends LocationResponseGenerator(Status.MovedPermanently) { type T[F[_]] = MOVEDPERMANENTLY[F] }
  object Found                         extends LocationResponseGenerator(Status.Found) { type T[F[_]] = FOUND[F] }
  object SeeOther                      extends LocationResponseGenerator(Status.SeeOther) { type T[F[_]] = SEEOTHER[F] }
  object NotModified                   extends EmptyResponseGenerator(Status.NotModified) { type T[F[_]] = NOTMODIFIED[F] }
  // UseProxy has been deprecated in RFC7231 so it is omitted.
  object TemporaryRedirect             extends LocationResponseGenerator(Status.TemporaryRedirect) { type T[F[_]] = TEMPORARYREDIRECT[F] }
  object PermanentRedirect             extends LocationResponseGenerator(Status.PermanentRedirect) { type T[F[_]] = PERMANENTREDIRECT[F] }

  object BadRequest                    extends EntityResponseGenerator(Status.BadRequest) { type T[F[_], A] = BADREQUEST[F, A] }
  object Unauthorized                  extends EntityResponseGenerator(Status.Unauthorized) { type T[F[_], A] = UNAUTHORIZED[F, A] }
  object PaymentRequired               extends EntityResponseGenerator(Status.PaymentRequired) { type T[F[_], A] = PAYMENTREQUIRED[F, A] }
  object Forbidden                     extends EntityResponseGenerator(Status.Forbidden) { type T[F[_], A] = FORBIDDEN[F, A] }
  object NotFound                      extends EntityResponseGenerator(Status.NotFound) { type T[F[_], A] = NOTFOUND[F, A] }
  object MethodNotAllowed              extends EntityResponseGenerator(Status.MethodNotAllowed) { type T[F[_], A] = METHODNOTALLOWED[F, A] }
  object NotAcceptable                 extends EntityResponseGenerator(Status.NotAcceptable) { type T[F[_], A] = NOTACCEPTABLE[F, A] }
  object ProxyAuthenticationRequired   extends EntityResponseGenerator(Status.ProxyAuthenticationRequired) { type T[F[_], A] = PROXYAUTHENTICATIONREQUIRED[F, A] }
  object RequestTimeout                extends EntityResponseGenerator(Status.RequestTimeout) { type T[F[_], A] = REQUESTTIMEOUT[F, A] }
  object Conflict                      extends EntityResponseGenerator(Status.Conflict) { type T[F[_], A] = CONFLICT[F, A] }
  object Gone                          extends EntityResponseGenerator(Status.Gone) { type T[F[_], A] = GONE[F, A] }
  object LengthRequired                extends EntityResponseGenerator(Status.LengthRequired) { type T[F[_], A] = LENGTHREQUIRED[F, A] }
  object PreconditionFailed            extends EntityResponseGenerator(Status.PreconditionFailed) { type T[F[_], A] = PRECONDITIONFAILED[F, A] }
  object PayloadTooLarge               extends EntityResponseGenerator(Status.PayloadTooLarge) { type T[F[_], A] = PAYLOADTOOLARGE[F, A] }
  object UriTooLong                    extends EntityResponseGenerator(Status.UriTooLong) { type T[F[_], A] = URITOOLONG[F, A] }
  object UnsupportedMediaType          extends EntityResponseGenerator(Status.UnsupportedMediaType) { type T[F[_], A] = UNSUPPORTEDMEDIATYPE[F, A] }
  object RangeNotSatisfiable           extends EntityResponseGenerator(Status.RangeNotSatisfiable) { type T[F[_], A] = RANGENOTSATISFIABLE[F, A] }
  object ExpectationFailed             extends EntityResponseGenerator(Status.ExpectationFailed) { type T[F[_], A] = EXPECTATIONFAILED[F, A] }
  object UnprocessableEntity           extends EntityResponseGenerator(Status.UnprocessableEntity) { type T[F[_], A] = UNPROCESSABLEENTITY[F, A] }
  object Locked                        extends EntityResponseGenerator(Status.Locked) { type T[F[_], A] = LOCKED[F, A] }
  object FailedDependency              extends EntityResponseGenerator(Status.FailedDependency) { type T[F[_], A] = FAILEDDEPENDENCY[F, A] }
  object UpgradeRequired               extends EntityResponseGenerator(Status.UpgradeRequired) { type T[F[_], A] = UPGRADEREQUIRED[F, A] }
  object PreconditionRequired          extends EntityResponseGenerator(Status.PreconditionRequired) { type T[F[_], A] = PRECONDITIONREQUIRED[F, A] }
  object TooManyRequests               extends EntityResponseGenerator(Status.TooManyRequests) { type T[F[_], A] = TOOMANYREQUESTS[F, A] }
  object RequestHeaderFieldsTooLarge   extends EntityResponseGenerator(Status.RequestHeaderFieldsTooLarge) { type T[F[_], A] = REQUESTHEADERFIELDSTOOLARGE[F, A] }

  object InternalServerError           extends EntityResponseGenerator(Status.InternalServerError) { type T[F[_], A] = INTERNALSERVERERROR[F, A] }
  object NotImplemented                extends EntityResponseGenerator(Status.NotImplemented) { type T[F[_], A] = NOTIMPLEMENTED[F, A] }
  object BadGateway                    extends EntityResponseGenerator(Status.BadGateway) { type T[F[_], A] = BADGATEWAY[F, A] }
  object ServiceUnavailable            extends EntityResponseGenerator(Status.ServiceUnavailable) { type T[F[_], A] = SERVICEUNAVAILABLE[F, A] }
  object GatewayTimeout                extends EntityResponseGenerator(Status.GatewayTimeout) { type T[F[_], A] = GATEWAYTIMEOUT[F, A] }
  object HttpVersionNotSupported       extends EntityResponseGenerator(Status.HttpVersionNotSupported) { type T[F[_], A] = HTTPVERSIONNOTSUPPORTED[F, A] }
  object VariantAlsoNegotiates         extends EntityResponseGenerator(Status.VariantAlsoNegotiates) { type T[F[_], A] = VARIANTALSONEGOTIATES[F, A] }
  object InsufficientStorage           extends EntityResponseGenerator(Status.InsufficientStorage) { type T[F[_], A] = INSUFFICIENTSTORAGE[F, A] }
  object LoopDetected                  extends EntityResponseGenerator(Status.LoopDetected) { type T[F[_], A] = LOOPDETECTED[F, A] }
  object NotExtended                   extends EntityResponseGenerator(Status.NotExtended) { type T[F[_], A] = NOTEXTENDED[F, A] }
  object NetworkAuthenticationRequired extends EntityResponseGenerator(Status.NetworkAuthenticationRequired) { type T[F[_], A] = NETWORKAUTHENTICATIONREQUIRED[F, A] }
}
