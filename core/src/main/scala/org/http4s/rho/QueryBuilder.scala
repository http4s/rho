package org.http4s
package rho

import org.http4s.rho.bits.{AsTypedQuery, QueryReader, HeaderAppendable, UriConverter}
import bits.PathAST._
import bits.QueryAST._
import bits.HeaderAST._

import shapeless.ops.hlist.Prepend
import shapeless.{HNil, ::, HList}

case class QueryBuilder[T <: HList](method: Method,
                                      path: PathRule,
                                     query: QueryRule)
  extends RouteExecutable[T]
  with HeaderAppendable[T]
  with UriConvertible
  with RoutePrependable[QueryBuilder[T]]
{
  override def /:(prefix: TypedPath[HNil]): QueryBuilder[T] =
    new QueryBuilder[T](method, PathAnd(prefix.rule, path), query)
  
  override type HeaderAppendResult[T <: HList] = Router[T]

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(Router(method, path, query, headers), action)

  override def >>>[T1 <: HList](v: TypedHeader[T1])(implicit prep1: Prepend[T1, T]): Router[prep1.Out] =
    Router(method, path, query, v.rule)

  final def &[QueryType, T1 <: HList](q: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prep: Prepend[T1, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, QueryAnd(query, ev(q).rule))

  override def headers: HeaderRule = EmptyHeaderRule

  private val uriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(query)
    } yield UriTemplate(path = p, query = q)

  override def asUriTemplate(request: Request) =
    UriConvertible.respectPathInfo(uriTemplate, request)
}
