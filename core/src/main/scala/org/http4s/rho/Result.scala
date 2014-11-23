package org.http4s
package rho

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
](resp: Response)

object Result {
  type BaseResult = Result[Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any,Any]
  type TopResult  = Result[Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing ,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
  type ExResult   = Result[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_]
}

import Result._

//
//import scala.language.implicitConversions
//
//import org.http4s.Writable.Entity
//
import scalaz.concurrent.Task
import scalaz.stream.Process
//
//case class Result[S, T](resp: Response) extends AnyVal
//
//sealed trait EmptyResult
//
//object EmptyResult {
//  implicit val emptyResultWritable: Writable[EmptyResult] = Writable(
//    _ => Task.now(Entity(Process.halt, None)), Headers.empty
//  )
//}
//
trait ResultSyntaxInstances {

  implicit class ResultSyntax[T >: Result.TopResult <: BaseResult](r: T) extends MessageOps {
    override type Self = T

    override def withAttribute[A](key: AttributeKey[A], value: A): Self =
      Result(r.resp.withAttribute(key, value))

    override def withHeaders(headers: Headers): Self =
      Result(r.resp.withHeaders(headers))

    override def putHeaders(headers: Header*): Self =
      Result(r.resp.putHeaders(headers:_*))

    override def filterHeaders(f: (Header) => Boolean): Self =
      Result(r.resp.filterHeaders(f))

    def withBody[T](b: T)(implicit w: Writable[T]): Task[Self] = {
      r.resp.withBody(b)(w).map(Result(_))
    }
  }

  implicit class TaskResultSyntax[T >: Result.TopResult <: BaseResult](r: Task[T]) extends MessageOps {
    override type Self = Task[T]

    override def withAttribute[A](key: AttributeKey[A], value: A): Self =
      r.map(r => Result(r.resp.withAttribute(key, value)))

    override def withHeaders(headers: Headers): Self =
      r.map(r => Result(r.resp.withHeaders(headers)))

    override def putHeaders(headers: Header*): Self =
      r.map(r => Result(r.resp.putHeaders(headers:_*)))

    override def filterHeaders(f: (Header) => Boolean): Self =
      r.map(r => Result(r.resp.filterHeaders(f)))

    def withBody[T](b: T)(implicit w: Writable[T]): Self = {
      r.flatMap(_.withBody(b)(w))
    }
  }
}
