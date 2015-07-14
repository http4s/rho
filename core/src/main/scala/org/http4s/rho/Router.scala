package org.http4s
package rho

import bits.PathAST._
import bits.HeaderAST._
import bits.QueryAST.QueryRule
import org.http4s.rho.bits.{HeaderAppendable, HListToFunc}
import headers.`Content-Type`

import scala.reflect.runtime.universe.{Type, TypeTag}

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
                          with Decodable[T, Nothing]
{
  override type HeaderAppendResult[T <: HList] = Router[T]

  override def >>>[T2 <: HList](v: TypedHeader[T2])(implicit prep1: Prepend[T2, T]): Router[prep1.Out] =
    Router(method, path, query, HeaderAnd(headers, v.rule))

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(this, action)

  override def decoding[R](decoder: EntityDecoder[R])(implicit t: TypeTag[R]): CodecRouter[T, R] =
    CodecRouter(this, decoder)
}

case class CodecRouter[T <: HList, R](router: Router[T], decoder: EntityDecoder[R])(implicit t: TypeTag[R])
           extends HeaderAppendable[T]
           with RouteExecutable[R::T]
           with RoutingEntity[R::T]
           with Decodable[T, R]
{
  override type HeaderAppendResult[T <: HList] = CodecRouter[T, R]

  override def >>>[T2 <: HList](v: TypedHeader[T2])(implicit prep1: Prepend[T2, T]): CodecRouter[prep1.Out,R] =
    CodecRouter(router >>> v, decoder)

  /** Append the header to the builder, generating a new typed representation of the route */
//  override def >>>[T2 <: HList](header: TypedHeader[T2])(implicit prep: Prepend[T2, T]): CodecRouter[prep.Out, R] = ???

  override def makeRoute(action: Action[R::T]): RhoRoute[R::T] = RhoRoute(this, action)

  override def path: PathRule = router.path

  override def method: Method = router.method

  override def query: QueryRule = router.query

  override def decoding[R2 >: R](decoder2: EntityDecoder[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] =
    CodecRouter(router, decoder orElse decoder2)

  override val headers: HeaderRule = router.headers

  def entityType: Type = t.tpe
}

