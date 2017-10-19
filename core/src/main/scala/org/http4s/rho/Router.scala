package org.http4s
package rho

import bits.PathAST._
import cats.Functor
import org.http4s.rho.bits.{HeaderAppendable, TypedHeader}
import org.http4s.rho.bits.RequestAST.{AndRule, RequestRule}

import scala.reflect.runtime.universe.{Type, TypeTag}
import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.Prepend

import scala.reflect.runtime.universe

sealed trait RoutingEntity[F[_], T <: HList] {
  type Self <: RoutingEntity[F, T]

  def method: Method
  def path: PathRule
  def rules: RequestRule[F]

  def /:(path: TypedPath[F, HNil]): Self
  def withMethod(method: Method): Self
}

/** Provides the operations for generating a router
  *
  * @param method request methods to match
  * @param path path matching stack
  * @param rules header validation stack
  * @tparam T cumulative type of the required method for executing the router
  */
case class Router[F[_], T <: HList](method: Method,
                                    path: PathRule,
                                    rules: RequestRule[F])
  extends RouteExecutable[F, T]
    with HeaderAppendable[F, T]
    with RoutingEntity[F, T]
    with Decodable[F, T, Nothing]
    with RoutePrependable[F, Router[F, T]]
{
  type Self = Router[F, T]

  override type HeaderAppendResult[T <: HList] = Router[F, T]

  override def /:(prefix: TypedPath[F, HNil]): Self = {
    copy(path = PathAnd(prefix.rule, path))
  }

  override def >>>[T1 <: HList](v: TypedHeader[F, T1])(implicit prep1: Prepend[T1, T]): Router[F, prep1.Out] =
    Router(method, path, AndRule(rules, v.rule))

  override def makeRoute(action: Action[F, T]): RhoRoute[F, T] = RhoRoute(this, action)

  override def decoding[R](decoder: EntityDecoder[F, R])(implicit F: Functor[F], t: TypeTag[R]): CodecRouter[F, T, R] =
    CodecRouter(this, decoder)

  def withMethod(other: Method): Self =
    copy(method = other)
}

case class CodecRouter[F[_], T <: HList, R](router: Router[F, T], decoder: EntityDecoder[F, R])(implicit t: TypeTag[R])
  extends HeaderAppendable[F, T]
    with RouteExecutable[F, R::T]
    with RoutingEntity[F, R::T]
    with Decodable[F, T, R]
{
  type Self = CodecRouter[F, T, R]

  override type HeaderAppendResult[TU <: HList] = CodecRouter[F, TU, R]

  override def >>>[T1 <: HList](v: TypedHeader[F, T1])(implicit prep1: Prepend[T1, T]): CodecRouter[F, prep1.Out, R] =
    CodecRouter(router >>> v, decoder)

  override def /:(prefix: TypedPath[F, HNil]): Self =
    copy(router = prefix /: router)

  /** Append the header to the builder, generating a new typed representation of the route */
  //  override def >>>[T2 <: HList](header: TypedHeader[T2])(implicit prep: Prepend[T2, T]): CodecRouter[prep.Out, R] = ???

  override def makeRoute(action: Action[F, R::T]): RhoRoute[F, R::T] = RhoRoute(this, action)

  override val path: PathRule = router.path

  override def method: Method = router.method

  override val rules: RequestRule[F] = router.rules

  override def decoding[R2 >: R](decoder2: EntityDecoder[F, R2])(implicit F: Functor[F], t: TypeTag[R2]): CodecRouter[F, T, R2] =
    CodecRouter(router, decoder orElse decoder2)

  def entityType: Type = t.tpe

  def withMethod(other: Method): Self =
    copy(router = router.withMethod(other))
}

