package org.http4s
package rho

import bits.{HeaderAppendable, UriConverter }
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

  def &[T1 <: HList](q: TypedQuery[T1])(implicit prep: Prepend[T1, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, QueryAnd(query, q.rule))

  override def headers: HeaderRule = EmptyHeaderRule

  private val uriTemplate =
    for {
      p <- UriConverter.createPath(path)
      q <- UriConverter.createQuery(query)
    } yield UriTemplate(path = p, query = q)

  override def asUriTemplate(request: Request) =
    UriConvertible.respectPathInfo(uriTemplate, request)
}
