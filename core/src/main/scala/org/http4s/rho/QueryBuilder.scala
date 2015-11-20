package org.http4s
package rho

import bits.HeaderAppendable
import bits.PathAST._
import bits.QueryAST._
import bits.HeaderAST._
import org.http4s.rho.bits.RequestAST.{AndRule, RequestRule}

import shapeless.ops.hlist.Prepend
import shapeless.{HNil, ::, HList}

case class QueryBuilder[T <: HList](method: Method,
                                      path: PathRule,
                                     rules: RequestRule)
  extends RouteExecutable[T]
  with HeaderAppendable[T]
  with UriConvertible
  with RoutePrependable[QueryBuilder[T]]
{
  override def /:(prefix: TypedPath[HNil]): QueryBuilder[T] =
    new QueryBuilder[T](method, PathAnd(prefix.rule, path), rules)
  
  override type HeaderAppendResult[T <: HList] = Router[T]

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(Router(method, path, rules), action)

  override def >>>[T1 <: HList](rule: TypedHeader[T1])(implicit prep1: Prepend[T1, T]): Router[prep1.Out] =
    Router(method, path, AndRule(rules, rule.rule))

  def &[T1 <: HList](rule: TypedQuery[T1])(implicit prep: Prepend[T1, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, AndRule(rules, rule.rule))
}
