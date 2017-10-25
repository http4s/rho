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
  type UNAVAILABLEFORLEGALREASONS[F[_], A]    = Result[F, Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,  A    ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
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


  // 1xx
  object Continue {
    def apply[F[_]] = new EmptyResponseGenerator[F](Status.Continue) {
      type T = CONTINUE[F]
    }
  }

  object SwitchingProtocols {
    def apply[F[_]] = new EmptyResponseGenerator[F](Status.SwitchingProtocols) {
      type T = SWITCHINGPROTOCOLS[F]
    }
  }

  object Processing {
    def apply[F[_]] = new EmptyResponseGenerator[F](Status.Processing) {
      type T = PROCESSING[F]
    }
  }

  // 2xx
  object Ok {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Ok) {
      type T[A] = OK[F, A]
    }
  }

  object Created {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Created) {
      type T[A] = CREATED[F, A]
    }
  }

  object Accepted {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Accepted) {
      type T[A] = ACCEPTED[F, A]
    }
  }

  object NonAuthoritativeInformation {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.NonAuthoritativeInformation) {
      type T[A] = NONAUTHORITATIVEINFORMATION[F, A]
    }
  }

  object NoContent {
    def apply[F[_]] = new EmptyResponseGenerator[F](Status.NoContent) {
      type T = NOCONTENT[F]
    }
  }

  object ResetContent {
    def apply[F[_]] = new EmptyResponseGenerator[F](Status.ResetContent) {
      type T = RESETCONTENT[F]
    }
  }

  object PartialContent {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.PartialContent) {
      type T[A] = PARTIALCONTENT[F, A]
    }
  }

  object MultiStatus {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.MultiStatus) {
      type T[A] = MULTISTATUS[F, A]
    }
  }

  object AlreadyReported {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.AlreadyReported) {
      type T[A] = ALREADYREPORTED[F, A]
    }
  }

  object IMUsed {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.IMUsed) {
      type T[A] = IMUSED[F, A]
    }
  }

  // 3xx
  object MultipleChoices {
    def apply[F[_]] = new LocationResponseGenerator[F](Status.MultipleChoices) {
      type T = MULTIPLECHOICES[F]
    }
  }

  object MovedPermanently {
    def apply[F[_]] = new LocationResponseGenerator[F](Status.MovedPermanently) {
      type T = MOVEDPERMANENTLY[F]
    }
  }

  object Found {
    def apply[F[_]] = new LocationResponseGenerator[F](Status.Found) {
      type T = FOUND[F]
    }
  }

  object SeeOther {
    def apply[F[_]] = new LocationResponseGenerator[F](Status.SeeOther) {
      type T = SEEOTHER[F]
    }
  }

  object NotModified {
    def apply[F[_]] = new EmptyResponseGenerator[F](Status.NotModified) {
      type T = NOTMODIFIED[F]
    }
  }

  // UseProxy has been deprecated in RFC7231 so it is omitted.
  object TemporaryRedirect {
    def apply[F[_]] = new LocationResponseGenerator[F](Status.TemporaryRedirect) {
      type T = TEMPORARYREDIRECT[F]
    }
  }

  object PermanentRedirect {
    def apply[F[_]] = new LocationResponseGenerator[F](Status.PermanentRedirect) {
      type T = PERMANENTREDIRECT[F]
    }
  }

  // 4xx
  object BadRequest {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.BadRequest) {
      type T[A] = BADREQUEST[F, A]
    }
  }

  object Unauthorized {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Unauthorized) {
      type T[A] = UNAUTHORIZED[F, A]
    }
  }

  object PaymentRequired {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.PaymentRequired) {
      type T[A] = PAYMENTREQUIRED[F, A]
    }
  }

  object Forbidden {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Forbidden) {
      type T[A] = FORBIDDEN[F, A]
    }
  }

  object NotFound {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.NotFound) {
      type T[A] = NOTFOUND[F, A]
    }
  }

  object MethodNotAllowed {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.MethodNotAllowed) {
      type T[A] = METHODNOTALLOWED[F, A]
    }
  }

  object NotAcceptable {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.NotAcceptable) {
      type T[A] = NOTACCEPTABLE[F, A]
    }
  }

  object ProxyAuthenticationRequired {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.ProxyAuthenticationRequired) {
      type T[A] = PROXYAUTHENTICATIONREQUIRED[F, A]
    }
  }

  object RequestTimeout {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.RequestTimeout) {
      type T[A] = REQUESTTIMEOUT[F, A]
    }
  }

  object Conflict {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Conflict) {
      type T[A] = CONFLICT[F, A]
    }
  }

  object Gone {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Gone) {
      type T[A] = GONE[F, A]
    }
  }

  object LengthRequired {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.LengthRequired) {
      type T[A] = LENGTHREQUIRED[F, A]
    }
  }

  object PreconditionFailed {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.PreconditionFailed) {
      type T[A] = PRECONDITIONFAILED[F, A]
    }
  }

  object PayloadTooLarge {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.PayloadTooLarge) {
      type T[A] = PAYLOADTOOLARGE[F, A]
    }
  }

  object UriTooLong {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.UriTooLong) {
      type T[A] = URITOOLONG[F, A]
    }
  }

  object UnsupportedMediaType {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.UnsupportedMediaType) {
      type T[A] = UNSUPPORTEDMEDIATYPE[F, A]
    }
  }

  object RangeNotSatisfiable {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.RangeNotSatisfiable) {
      type T[A] = RANGENOTSATISFIABLE[F, A]
    }
  }

  object ExpectationFailed {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.ExpectationFailed) {
      type T[A] = EXPECTATIONFAILED[F, A]
    }
  }

  object UnprocessableEntity {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.UnprocessableEntity) {
      type T[A] = UNPROCESSABLEENTITY[F, A]
    }
  }

  object Locked {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.Locked) {
      type T[A] = LOCKED[F, A]
    }
  }

  object FailedDependency {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.FailedDependency) {
      type T[A] = FAILEDDEPENDENCY[F, A]
    }
  }

  object UpgradeRequired {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.UpgradeRequired) {
      type T[A] = UPGRADEREQUIRED[F, A]
    }
  }

  object PreconditionRequired {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.PreconditionRequired) {
      type T[A] = PRECONDITIONREQUIRED[F, A]
    }
  }

  object TooManyRequests {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.TooManyRequests) {
      type T[A] = TOOMANYREQUESTS[F, A]
    }
  }

  object RequestHeaderFieldsTooLarge {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.RequestHeaderFieldsTooLarge) {
      type T[A] = REQUESTHEADERFIELDSTOOLARGE[F, A]
    }
  }

  object UnavailableForLegalReasons {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.UnavailableForLegalReasons) {
      type T[A] = UNAVAILABLEFORLEGALREASONS[F, A]
    }
  }

  // 5xx
  object InternalServerError {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.InternalServerError) {
      type T[A] = INTERNALSERVERERROR[F, A]
    }
  }

  object NotImplemented {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.NotImplemented) {
      type T[A] = NOTIMPLEMENTED[F, A]
    }
  }

  object BadGateway {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.BadGateway) {
      type T[A] = BADGATEWAY[F, A]
    }
  }

  object ServiceUnavailable {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.ServiceUnavailable) {
      type T[A] = SERVICEUNAVAILABLE[F, A]
    }
  }

  object GatewayTimeout {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.GatewayTimeout) {
      type T[A] = GATEWAYTIMEOUT[F, A]
    }
  }

  object HttpVersionNotSupported {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.HttpVersionNotSupported) {
      type T[A] = HTTPVERSIONNOTSUPPORTED[F, A]
    }
  }

  object VariantAlsoNegotiates {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.VariantAlsoNegotiates) {
      type T[A] = VARIANTALSONEGOTIATES[F, A]
    }
  }

  object InsufficientStorage {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.InsufficientStorage) {
      type T[A] = INSUFFICIENTSTORAGE[F, A]
    }
  }

  object LoopDetected {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.LoopDetected) {
      type T[A] = LOOPDETECTED[F, A]
    }
  }

  object NotExtended {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.NotExtended) {
      type T[A] = NOTEXTENDED[F, A]
    }
  }

  object NetworkAuthenticationRequired {
    def apply[F[_]] = new EntityResponseGenerator[F](Status.NetworkAuthenticationRequired) {
      type T[A] = NETWORKAUTHENTICATIONREQUIRED[F, A]
    }
  }
}
