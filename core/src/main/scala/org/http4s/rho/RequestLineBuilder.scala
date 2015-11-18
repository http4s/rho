package org.http4s
package rho

import org.http4s.rho.bits.AsTypedQuery
import org.http4s.rho.bits.QueryAST.{ QueryAnd, QueryRule }

import bits.HeaderAST.{ EmptyHeaderRule, HeaderRule }
import org.http4s.rho.bits.PathAST.{PathAnd, TypedPath, PathRule}
import shapeless.{HNil, HList}
import shapeless.ops.hlist.Prepend

case class RequestLineBuilder[T <: HList](path: PathRule, query: QueryRule)
  extends TypedBuilder[T]
  with RoutePrependable[RequestLineBuilder[T]]
  with UriConvertible {

  override def headers: HeaderRule = EmptyHeaderRule

  override def /:(prefix: TypedPath[HNil]): RequestLineBuilder[T] =
    copy(path = PathAnd(prefix.rule, path))

  def &[QueryType, T1 <: HList](q: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prep: Prepend[T1, T]): RequestLineBuilder[prep.Out] =
    RequestLineBuilder(path, QueryAnd(query, ev(q).rule))
}
