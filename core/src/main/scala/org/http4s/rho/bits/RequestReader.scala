package org.http4s.rho
package bits

import scala.language.existentials

import org.http4s.{HeaderKey, Request}
import org.http4s.rho.bits.RequestReader.CaptureParams

import scalaz.Applicative

import scala.reflect.runtime.universe.TypeTag

sealed trait RequestReader[T] { self =>
  def read(req: Request): ResultResponse[T]

  def parameters: Seq[CaptureParams]
}

object RequestReader {
  final case class ExtractingReader[T](f: Request => ResultResponse[T], meta: CaptureParams) extends RequestReader[T] {
    override def read(req: Request): ResultResponse[T] = f(req)
    override val parameters: Seq[CaptureParams] = meta::Nil
  }

  sealed trait CaptureParams
  final case class QueryCaptureParams(name: String, optional: Boolean, tag: TypeTag[_]) extends CaptureParams
  final case class HeaderCaptureParams(key: HeaderKey.Extractable, optional: Boolean, tag: TypeTag[_]) extends CaptureParams

  private[rho] def queryNames(reader: RequestReader[_]): Seq[String] =
    reader.parameters.collect { case QueryCaptureParams(name,_,_) => name }

  private case class PureRequestReader[T](value: T) extends RequestReader[T] {
    override def read(req: Request): ResultResponse[T] = valueResp
    override def parameters: Seq[CaptureParams] = Nil

    private val valueResp = SuccessResponse(value)
  }

  def point[A](a: A): RequestReader[A] = PureRequestReader(a)

  implicit val instance: Applicative[RequestReader] = new Applicative[RequestReader] {
    override def point[A](a: => A): RequestReader[A] = PureRequestReader(a)

    override def ap[A, B](fa: => RequestReader[A])(f: => RequestReader[(A) => B]): RequestReader[B] = {
      val sfa = fa  // we don't want these lazy for performance reasons
      val sf = f

      new RequestReader[B] {
        override def read(req: Request): ResultResponse[B] = {
          sf.read(req).flatMap(f => sfa.read(req).map(f))
        }

        override val parameters: Seq[CaptureParams] = sfa.parameters ++ sf.parameters
      }
    }
  }
}
