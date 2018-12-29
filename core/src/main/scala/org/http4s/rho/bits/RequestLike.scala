package org.http4s.rho.bits

import org.http4s.{AttributeMap, AuthedRequest, EntityBody, Headers, HttpVersion, Method, Request, Uri}

case class RequestLikeOps[R[_[_]], F[_]](r: R[F]) extends AnyVal {
  def toRequest(implicit rl: RequestLike[R]): Request[F] = rl.toRequest(r)
  def method(implicit rl: RequestLike[R]): Method = rl.method(r)
  def uri(implicit rl: RequestLike[R]): Uri = rl.uri(r)
  def httpVersion(implicit rl: RequestLike[R]): HttpVersion = rl.httpVersion(r)
  def headers(implicit rl: RequestLike[R]): Headers = rl.headers(r)
  def body(implicit rl: RequestLike[R]): EntityBody[F] = rl.body(r)
  def attributes(implicit rl: RequestLike[R]): AttributeMap = rl.attributes(r)
  def pathInfo(implicit rl: RequestLike[R]): String = rl.pathInfo(r)
}

trait RequestLike[R[_[_]]] {
  def toRequest[F[_]](r: R[F]): Request[F]

  def method[F[_]](r: R[F]): Method
  def uri[F[_]](r: R[F]): Uri
  def httpVersion[F[_]](r: R[F]): HttpVersion
  def headers[F[_]](r: R[F]): Headers
  def body[F[_]](r: R[F]): EntityBody[F]
  def attributes[F[_]](r: R[F]): AttributeMap

  def pathInfo[F[_]](r: R[F]): String
}

object RequestLike {
  def apply[R[_[_]]](implicit r: RequestLike[R]): RequestLike[R] = r

  implicit val request: RequestLike[Request] =
    new RequestLike[Request] {
      override def toRequest[F[_]](r: Request[F]): Request[F] = r

      override def method[F[_]](r: Request[F]): Method = r.method
      override def uri[F[_]](r: Request[F]): Uri = r.uri
      override def httpVersion[F[_]](r: Request[F]): HttpVersion = r.httpVersion
      override def headers[F[_]](r: Request[F]): Headers = r.headers
      override def body[F[_]](r: Request[F]): EntityBody[F] = r.body
      override def attributes[F[_]](r: Request[F]): AttributeMap = r.attributes

      override def pathInfo[F[_]](r: Request[F]): String = r.pathInfo
    }

  implicit def authedRequest[U]: RequestLike[AuthedRequest[?[_], U]] =
    new RequestLike[AuthedRequest[?[_], U]] {
      override def toRequest[F[_]](r: AuthedRequest[F, U]): Request[F] = r.req

      override def method[F[_]](r: AuthedRequest[F, U]): Method = r.req.method
      override def uri[F[_]](r: AuthedRequest[F, U]): Uri = r.req.uri
      override def httpVersion[F[_]](r: AuthedRequest[F, U]): HttpVersion = r.req.httpVersion
      override def headers[F[_]](r: AuthedRequest[F, U]): Headers = r.req.headers
      override def body[F[_]](r: AuthedRequest[F, U]): EntityBody[F] = r.req.body
      override def attributes[F[_]](r: AuthedRequest[F, U]): AttributeMap = r.req.attributes

      override def pathInfo[F[_]](r: AuthedRequest[F, U]): String = r.req.pathInfo
    }

  implicit def ops[R[_[_]]: RequestLike, F[_]](r: R[F]): RequestLikeOps[R, F] =
    RequestLikeOps[R, F](r)
}
