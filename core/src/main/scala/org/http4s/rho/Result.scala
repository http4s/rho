package org.http4s
package rho

import scala.language.implicitConversions

import org.http4s.Header.`Content-Length`

import scalaz.concurrent.Task

case class Result[T](resp: Response) extends AnyVal

trait ResultSyntax {

  implicit class ResultSyntax[T](r: Result[T]) extends MessageOps {
    override type Self = Result[T]

    override def withAttribute[A](key: AttributeKey[A], value: A): Self =
      Result[T](r.resp.withAttribute(key, value))

    override def withHeaders(headers: Headers): Self =
      Result(r.resp.withHeaders(headers))

    override def putHeaders(headers: Header*): Self =
      Result(r.resp.putHeaders(headers:_*))

    override def filterHeaders(f: (Header) => Boolean): Self =
      Result(r.resp.filterHeaders(f))
  }

  implicit class TaskResultSyntax[T](r: Task[Result[T]]) extends MessageOps {
    override type Self = Task[Result[T]]

    override def withAttribute[A](key: AttributeKey[A], value: A): Self =
      r.map(r => Result(r.resp.withAttribute(key, value)))

    override def withHeaders(headers: Headers): Self =
      r.map(r => Result(r.resp.withHeaders(headers)))

    override def putHeaders(headers: Header*): Self =
      r.map(r => Result(r.resp.putHeaders(headers:_*)))

    override def filterHeaders(f: (Header) => Boolean): Self =
      r.map(r => Result(r.resp.filterHeaders(f)))
  }

//  implicit def resultToMessage[T](r: Task[Result[T]]): MessageSyntax.ResponseSyntax[Task[Result[T]]] =
//    new MessageSyntax.ResponseSyntax[Task[Result[T]]] {
//      override protected def translateMessage(f: (Response) => Response#Self): Task[Result[T]] =
//        r.map(res => Result(f(res.resp)))
//
//      override protected def translateWithTask(f: (Response) => Task[Response#Self]): Task[Response#Self] = {
//        r.flatMap(res => f(res.resp))
//      }
//    }
//

  private def mkResult[R](s: Status, r: R, w: Writable[R]): Task[Result[R]] = {
    w.toEntity(r).map { entity =>
      val h = entity.length match {
        case Some(l) => w.headers.put(`Content-Length`(l))
        case None    => w.headers
      }

      Result(Response(status = s, body = entity.body, headers = h))
    }
  }

  def OK[R](r: R)(implicit w: Writable[R]) = mkResult(Status.Ok, r, w)

  def NotFound(path: String) = mkResult(Status.NotFound, path, Writable.stringWritable)
}
