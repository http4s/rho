package org.http4s
package rho

import scala.language.implicitConversions

import org.http4s.Writable.Entity

import scalaz.concurrent.Task
import scalaz.stream.Process

case class Result[T](resp: Response) extends AnyVal

sealed trait EmptyResult

object EmptyResult {
  implicit val emptyResultWritable: Writable[EmptyResult] = Writable(
    _ => Task.now(Entity(Process.halt, None)), Headers.empty
  )
}

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

    def withBody[T](b: T)(implicit w: Writable[T]): Task[Self] = {
      r.resp.withBody(b)(w).map(Result(_))
    }
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

    def withBody[T](b: T)(implicit w: Writable[T]): Self = {
      r.flatMap(_.withBody(b)(w))
    }
  }
}
