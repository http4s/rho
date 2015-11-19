package org.http4s
package rho

import bits.PathAST._
import org.http4s.rho.bits.AsRequestRule
import org.http4s.rho.bits.RequestRuleAST.{ RequestAnd, RequestRule }

import scala.reflect.runtime.universe.{Type, TypeTag}

import shapeless.{HNil, ::, HList}
import shapeless.ops.hlist.Prepend

sealed trait RoutingEntity[T <: HList] {
  def method: Method
  def path: PathRule
  def requestRules: RequestRule

  def /:(path: TypedPath[HNil]): RoutingEntity[T]
}

/** Provides the operations for generating a router
  *
  * @param method request methods to match
  * @param path path matching stack
  * @param requestRules request extraction rules
  * @tparam T cumulative type of the required method for executing the router
  */
case class Router[T <: HList](method: Method,
                              path: PathRule,
                              requestRules: RequestRule)
                       extends RouteExecutable[T]
                          with RoutingEntity[T]
                          with Decodable[T, Nothing]
                          with RoutePrependable[Router[T]]
{
  override def /:(prefix: TypedPath[HNil]): Router[T] = {
    copy(path = PathAnd(prefix.rule, path))
  }

  def &[ReqRule, T1 <: HList](rule: ReqRule)(implicit ev: AsRequestRule[ReqRule, T1], prep: Prepend[T1, T]): Router[prep.Out] =
    Router(method, path, RequestAnd(requestRules, ev(rule).rule))

  def >>>[ReqRule, T1 <: HList](rule: ReqRule)(implicit ev: AsRequestRule[ReqRule, T1], prep: Prepend[T1, T]): Router[prep.Out] =
    this & rule

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(this, action)

  override def decoding[R](decoder: EntityDecoder[R])(implicit t: TypeTag[R]): CodecRouter[T, R] =
    CodecRouter(this, decoder)
}

case class CodecRouter[T <: HList, R](router: Router[T], decoder: EntityDecoder[R])(implicit t: TypeTag[R])
           extends RouteExecutable[R::T]
           with RoutingEntity[R::T]
           with Decodable[T, R]
{

  def &[ReqRule, T1 <: HList](rule: ReqRule)(implicit ev: AsRequestRule[ReqRule, T1], prep: Prepend[T1, T]): CodecRouter[prep.Out, R] =
    CodecRouter(router & rule, decoder)

  def >>>[ReqRule, T1 <: HList](rule: ReqRule)(implicit ev: AsRequestRule[ReqRule, T1], prep1: Prepend[T1, T]): CodecRouter[prep1.Out,R] =
    this & rule

  override def /:(prefix: TypedPath[HNil]): CodecRouter[T, R] =
    copy(router = prefix /: router)

  /** Append the header to the builder, generating a new typed representation of the route */
//  override def >>>[T2 <: HList](header: TypedHeader[T2])(implicit prep: Prepend[T2, T]): CodecRouter[prep.Out, R] = ???

  override def makeRoute(action: Action[R::T]): RhoRoute[R::T] = RhoRoute(this, action)

  override def path: PathRule = router.path

  override def method: Method = router.method

  override def requestRules: RequestRule = router.requestRules

  override def decoding[R2 >: R](decoder2: EntityDecoder[R2])(implicit t: TypeTag[R2]): CodecRouter[T, R2] =
    CodecRouter(router, decoder orElse decoder2)

  def entityType: Type = t.tpe
}

