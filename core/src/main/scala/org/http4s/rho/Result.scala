package org.http4s
package rho

import org.http4s.Header.`Content-Length`

import scalaz.concurrent.Task

case class Result[T](resp: Response) extends AnyVal

trait ResultSyntax {

  implicit def resultToMessage[T](r: Result[T]): MessageSyntax.ResponseSyntax[Result[T]] =
    new MessageSyntax.ResponseSyntax[Result[T]] {
      override protected def translateMessage(f: (Response) => Response#Self): Result[T] =
        Result(f(r.resp))

      override protected def translateWithTask(f: (Response) => Task[Response#Self]): Task[Response#Self] =
        f(r.resp)
    }

  implicit def resultToMessage[T](r: Task[Result[T]]): MessageSyntax.ResponseSyntax[Task[Result[T]]] =
    new MessageSyntax.ResponseSyntax[Task[Result[T]]] {
      override protected def translateMessage(f: (Response) => Response#Self): Task[Result[T]] =
        r.map(res => Result(f(res.resp)))

      override protected def translateWithTask(f: (Response) => Task[Response#Self]): Task[Response#Self] = {
        r.flatMap(res => f(res.resp))
      }
    }


  private def mkResult[R](s: Status, r: R, w: Writable[R]): Task[Result[R]] = {
    w.toEntity(r).map { entity =>
      val h = entity.length match {
        case Some(l) => w.headers :+ `Content-Length`(l)
        case None    => w.headers
      }

      Result(Response(status = s, body = entity.body, headers = h))
    }
  }

  def OK[R](r: R)(implicit w: Writable[R]) = mkResult(Status.Ok, r, w)


}
