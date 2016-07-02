package org.http4s
package rho

import bits.PathAST._
import org.http4s.rho.bits.{TypedHeader, HeaderAppendable}
import org.http4s.rho.bits.RequestAST.{AndRule, RequestRule}

import scala.reflect.runtime.universe.{Type, TypeTag}

import shapeless.{HNil, ::, HList}
import shapeless.ops.hlist.Prepend
import scalaz.concurrent.Task

sealed trait RoutingEntity[T <: HList] {
  def method: Method
  def path: PathRule
  def rules: RequestRule

  def /:(path: TypedPath[HNil]): RoutingEntity[T]
}

/** Provides the operations for generating a router
  *
  * @param method request methods to match
  * @param path path matching stack
  * @param rules header validation stack
  * @tparam T cumulative type of the required method for executing the router
  */
case class Router[T <: HList](method: Method,
                              path: PathRule,
                              rules: RequestRule)
                       extends RouteExecutable[T]
                          with HeaderAppendable[T]
                          with RoutingEntity[T]
                          with Decodable[T, Nothing]
                          with RoutePrependable[Router[T]]
{
  override type HeaderAppendResult[T <: HList] = Router[T]

  override def /:(prefix: TypedPath[HNil]): Router[T] = {
    copy(path = PathAnd(prefix.rule, path))
  }

  override def >>>[T1 <: HList](v: TypedHeader[T1])(implicit prep1: Prepend[T1, T]): Router[prep1.Out] =
    Router(method, path, AndRule(rules, v.rule))

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(this, action)

  override def decoding[R](decoder: EntityDecoder[R])(implicit t: TypeTag[R]): CodecRouter[T, R] =
    CodecRouter(this, decoder, PartialFunction.empty) 
}

case class CodecRouter[T <: HList, R](router: Router[T], decoder: EntityDecoder[R], errHandler: PartialFunction[DecodeFailure, Task[Response]])(implicit t: TypeTag[R])
           extends HeaderAppendable[T]
           with RouteExecutable[R::T]
           with RoutingEntity[R::T]
           with Decodable[T, R]
{
  override type HeaderAppendResult[T <: HList] = CodecRouter[T, R]

  override def >>>[T1 <: HList](v: TypedHeader[T1])(implicit prep1: Prepend[T1, T]): CodecRouter[prep1.Out,R] =
    CodecRouter(router >>> v, decoder, errHandler)

  override def /:(prefix: TypedPath[HNil]): CodecRouter[T, R] =
    copy(router = prefix /: router)

  /** Append the header to the builder, generating a new typed representation of the route */
//  override def >>>[T2 <: HList](header: TypedHeader[T2])(implicit prep: Prepend[T2, T]): CodecRouter[prep.Out, R] = ???

  override def makeRoute(action: Action[R::T]): RhoRoute[R::T] = RhoRoute(this, action)

  override val path: PathRule = router.path

  override def method: Method = router.method

  override val rules: RequestRule = router.rules

  def handleError(otherErrHandler: PartialFunction[DecodeFailure, Task[Response]]): CodecRouter[T, R] = this.copy(errHandler = otherErrHandler)

  override def decoding[R2 >: R](decoder2: EntityDecoder[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] =
    CodecRouter(router, decoder orElse decoder2, errHandler)

  def entityType: Type = t.tpe
}

