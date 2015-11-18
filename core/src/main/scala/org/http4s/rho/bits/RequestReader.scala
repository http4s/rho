package org.http4s.rho
package bits


import org.http4s.rho.bits.QueryAST.{QueryCapture, TypedQuery}
import org.http4s.rho.bits.RequestReader.ExtractingReader
import shapeless.{ HNil, :: => :#: }

import scala.language.existentials

import org.http4s.{UriTemplate, Request}

import scalaz.Applicative

import scala.reflect.runtime.universe.TypeTag
import QueryReader.QueryCaptureParams

// Wrap the Applicative in a value class to narrow to the query
final class QueryReader[T](private val run: RequestReader[T, QueryCaptureParams])
  extends QueryCombinators[T:#:HNil] with UriConvertible
{
  def read(req: Request): ResultResponse[T] = run.read(req)

  def parameters: List[QueryCaptureParams] = run.parameters

  def asQueryRule: TypedQuery[T:#:HNil] = TypedQuery(QueryCapture(this))

  def names: List[String] = parameters.map(_.name)

  override def asUriTemplate(request: Request) =
    UriConvertible.respectPathInfo(uriTemplate, request)

  private def uriTemplate =
    for (q <- UriConverter.createQuery(QueryCapture(this)))
      yield UriTemplate(query = q)
}

object QueryReader {
  final case class QueryCaptureParams(name: String, default: Option[_], tag: TypeTag[_])

  implicit def queryReaderInstance[T]: AsTypedQuery[QueryReader[T], T:#:HNil] = new AsTypedQuery[QueryReader[T], T:#:HNil] {
    override def toQuery(t: QueryReader[T]): TypedQuery[T:#:HNil] = t.asQueryRule
  }

  def apply[T](params: List[QueryCaptureParams])(f: Map[String, Seq[String]] => ResultResponse[T]): QueryReader[T] = {
    new QueryReader[T](ExtractingReader(req => f(req.uri.multiParams), params))
  }

  implicit val instance: Applicative[QueryReader] = new Applicative[QueryReader] {
    override def point[A](a: => A): QueryReader[A] = new QueryReader(RequestReader.point(a))

    override def ap[A, B](fa: => QueryReader[A])(f: => QueryReader[(A) => B]): QueryReader[B] =
      new QueryReader(RequestReader.ap(fa.run)(f.run))
  }
}


private[bits] sealed trait RequestReader[T, +P] {
  def read(req: Request): ResultResponse[T]

  def parameters: List[P]
}

private[bits] object RequestReader {
  final case class ExtractingReader[T, P](f: Request => ResultResponse[T], parameters: List[P]) extends RequestReader[T, P] {
    override def read(req: Request): ResultResponse[T] = f(req)
  }

  private case class PureRequestReader[T](value: T) extends RequestReader[T, Nothing] {
    override def read(req: Request): ResultResponse[T] = valueResp
    override def parameters: List[Nothing] = Nil

    private val valueResp = SuccessResponse(value)
  }

  def point[A, P](a: A): RequestReader[A, P] = PureRequestReader(a)

  def ap[A, B, P](fa: => RequestReader[A, P])(f: => RequestReader[(A) => B, P]): RequestReader[B, P] = {
    val sfa = fa  // we don't want these lazy for performance reasons
    val sf = f

    new RequestReader[B, P] {
      override def read(req: Request): ResultResponse[B] = {
        sf.read(req).flatMap(f => sfa.read(req).map(f))
      }

      override val parameters: List[P] = sfa.parameters ::: sf.parameters
    }
  }
}