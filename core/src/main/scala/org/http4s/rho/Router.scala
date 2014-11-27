package org.http4s
package rho

import bits.PathAST._
import bits.HeaderAST._
import bits.QueryAST.QueryRule
import org.http4s.rho.bits.{HeaderAppendable, HListToFunc}

import shapeless.{::, HList}
import shapeless.ops.hlist.Prepend

sealed trait RoutingEntity[T <: HList] {
  def method: Method
  def path: PathRule
  def query: QueryRule
  def headers: HeaderRule
}

/** Provides the operations for generating a router
  *
  * @param method request methods to match
  * @param path path matching stack
  * @param headers header validation stack
  * @tparam T cumulative type of the required method for executing the router
  */
case class Router[T <: HList](method: Method,
                               val path: PathRule,
                               val query: QueryRule,
                               val headers: HeaderRule)
                       extends RouteExecutable[T]
                          with HeaderAppendable[T]
                          with RoutingEntity[T]
{

  type Self = Router[T]

  override def >>>[T2 <: HList](v: TypedHeader[T2])(implicit prep1: Prepend[T2, T]): Router[prep1.Out] =
    Router(method, path, query, HeaderAnd(headers, v.rule))

  override def makeAction[F](f: F, hf: HListToFunc[T, F]): RhoAction[T, F] =
    new RhoAction(this, f, hf)

  def ^[R](decoder: EntityDecoder[R]): CodecRouter[T, R] = decoding(decoder)

  def decoding[R](decoder: EntityDecoder[R]): CodecRouter[T, R] = CodecRouter(this, decoder)
}

case class CodecRouter[T <: HList, R](router: Router[T], decoder: EntityDecoder[R])
           extends HeaderAppendable[T]
           with RouteExecutable[R::T]
           with RoutingEntity[R::T]
{
  type Self = CodecRouter[T, R]

  override def >>>[T2 <: HList](v: TypedHeader[T2])(implicit prep1: Prepend[T2, T]): CodecRouter[prep1.Out,R] =
    CodecRouter(router >>> v, decoder)

  override def makeAction[F](f: F, hf: HListToFunc[R::T, F]): RhoAction[R::T, F] =
    new RhoAction(this, f, hf)

  override def path: PathRule = router.path

  override def method: Method = router.method

  override def query: QueryRule = router.query

  def ^[R2 >: R](decoder2: EntityDecoder[R2]): CodecRouter[T, R2] = decoding(decoder2)

  def decoding[R2 >: R](decoder2: EntityDecoder[R2]): CodecRouter[T, R2] = CodecRouter(router, decoder orElse decoder2)

  override val headers: HeaderRule = {
    if (!decoder.consumes.isEmpty) {
      val mt = requireThat(Header.`Content-Type`) { h: Header.`Content-Type`.HeaderT =>
        decoder.matchesMediaType(h.mediaType)
      }

      HeaderAnd(router.headers, mt.rule)
    } else router.headers
  }
}

