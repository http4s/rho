package org.http4s.rho

import org.http4s.rho.RequestReader.{HeaderCaptureParams, ExtractingReader, RequestParams, QueryCaptureParams}
import org.http4s.rho.bits.RequestRuleAST.{RequestCapture, TypedRequestRule}
import org.http4s.rho.bits.{AsRequestRule, RequestExtractorCombinators, ResultResponse, SuccessResponse}
import org.http4s.{HeaderKey, Headers, Request, UriTemplate}
import shapeless.{:: => :#:, HNil}

import scala.language.existentials
import scala.reflect.runtime.universe.TypeTag
import scala.util.Try
import scalaz.Applicative

object QueryReader {
  def apply[T](params: List[QueryCaptureParams])(f: Map[String, Seq[String]] => ResultResponse[T]): RequestReader[T] =
    RequestReader.extract(params)(req => f(req.uri.multiParams))
}

object HeaderReader {
  def apply[T](params: List[HeaderCaptureParams])(f: Headers => ResultResponse[T]): RequestReader[T] =
    RequestReader.extract(params)(req => f(req.headers))
}


sealed trait RequestReader[T] extends RequestExtractorCombinators[T:#:HNil] with UriConvertible {

  def read(req: Request): ResultResponse[T]

  def parameters: List[RequestParams]

  override def asQueryRule: TypedRequestRule[T:#:HNil] = TypedRequestRule(RequestCapture(this))

  override def asUriTemplate(request: Request): Try[UriTemplate] = asQueryRule.asUriTemplate(request)

  def queryNames: List[String] = parameters.collect{ case QueryCaptureParams(name,_,_) => name }
}

object RequestReader {

  // Types
  sealed trait RequestParams
  final case class QueryCaptureParams(name: String, default: Option[_], tag: TypeTag[_]) extends RequestParams
  final case class HeaderCaptureParams(key: HeaderKey.Extractable, tag: TypeTag[_]) extends RequestParams

  // Helper functions
  def apply[A](a: A): RequestReader[A] = PureRequestReader(a)

  def extract[T](parameters: List[RequestParams])(f: Request => ResultResponse[T]): RequestReader[T] =
    ExtractingReader(f, parameters)

  def apMap[A, B](fa: => RequestReader[A])(f: => RequestReader[(A) => B]): RequestReader[B] =
    RequestReaderAp(fa, f)

  // Instances
  implicit def typedRequestRuleInstance[T]: AsRequestRule[RequestReader[T], T:#:HNil] = new AsRequestRule[RequestReader[T], T:#:HNil]{
    override def toRule(t: RequestReader[T]): TypedRequestRule[T:#:HNil] = t.asQueryRule
  }

  implicit val reqReaderApp: Applicative[RequestReader] = new Applicative[RequestReader] {

    override def point[A](a: => A): RequestReader[A] = RequestReader(a)

    override def ap[A, B](fa: => RequestReader[A])(f: => RequestReader[(A) => B]): RequestReader[B] = apMap(fa)(f)
  }

  // Implementation details
  final private case class ExtractingReader[T](f: Request => ResultResponse[T], parameters: List[RequestParams]) extends RequestReader[T] {
    override def read(req: Request): ResultResponse[T] = f(req)
  }

  final private case class PureRequestReader[T](value: T) extends RequestReader[T] {
    override def read(req: Request): ResultResponse[T] = valueResp
    override def parameters: List[RequestParams] = Nil

    private val valueResp = SuccessResponse(value)
  }

  final private case class RequestReaderAp[F, T](a: RequestReader[F], ma: RequestReader[F => T]) extends RequestReader[T] {
    override def read(req: Request): ResultResponse[T] = ma.read(req).flatMap(a.read(req).map(_))

    override def parameters: List[RequestParams] = a.parameters ::: ma.parameters
  }
}