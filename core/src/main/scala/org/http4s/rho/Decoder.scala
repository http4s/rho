package org.http4s
package rho

import org.http4s.rho.bits.{ParserFailure, ParserResult, ParserSuccess}

import scala.language.implicitConversions
import scalaz.{-\/, \/-}

import scalaz.concurrent.Task

sealed trait Decoder[T] {

  def decode(req: Request): Task[ParserResult[T]]
  def consumes: Seq[MediaType]
  def force: Boolean
  def or(other: Decoder[T]): Decoder[T] = Decoder.OrDec(this, other)

  private[Decoder] def checkMediaType(req: Request): Boolean = {
    if (!consumes.isEmpty) {
      req.headers.get(Header.`Content-Type`).map {
        h =>
          consumes.find {
            m => m == h.mediaType
          }.isDefined
      }.getOrElse(false)
    }
    else false
  }
}

object Decoder {

  private type Result[T] = Task[ParserResult[T]]


  private case class BasicDecoder[T](decoder: Request => Result[T], consumes: Seq[MediaType], force: Boolean) extends Decoder[T] {
    override def decode(req: Request): Result[T] = decoder(req)
  }

  private case class OrDec[T](c1: Decoder[T], c2: Decoder[T]) extends Decoder[T] {

    override val consumes: Seq[MediaType] = c1.consumes ++ c2.consumes

    override def force: Boolean = c1.force || c2.force

    override def decode(req: Request): Result[T] = {
      if (c1.checkMediaType(req)) c1.decode(req)
      else if (c2.checkMediaType(req)) c2.decode(req)
      else if (c1.force) c1.decode(req)
      else if (c2.force) c2.decode(req)
      else Task.now(ParserFailure(s"No suitable codec found. Supported media types: ${consumes.mkString(", ")}"))
    }
  }

  implicit val strDec: Decoder[String] = {
    val dec: Request => Result[String] = _.body.runLog.map(vs => ParserSuccess(new String(vs.reduce(_ ++ _).toArray)))
    BasicDecoder(dec, MediaType.`text/plain`::Nil, true)
  }

  implicit def reqDecoder[T](f: Request => Task[T], mediaType: Seq[MediaType] = Nil, force: Boolean = true): Decoder[T] =
    BasicDecoder(f.andThen(_.attempt.map(_ match {
      case \/-(r) => ParserSuccess(r)
      case -\/(e) => ParserFailure(s"Decoder failure: $e")
    })), mediaType, force)

  implicit def bodyDecoder[T](f: HttpBody => Task[T]): Decoder[T] = reqDecoder(r => f(r.body))

}
