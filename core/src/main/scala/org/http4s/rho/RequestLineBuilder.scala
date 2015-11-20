package org.http4s
package rho

import org.http4s.rho.bits.QueryAST.TypedQuery
import org.http4s.rho.bits.PathAST.{PathAnd, TypedPath, PathRule}
import org.http4s.rho.bits.RequestAST.{AndRule, RequestRule}
import shapeless.{HNil, HList}
import shapeless.ops.hlist.Prepend

case class RequestLineBuilder[T <: HList](path: PathRule, rules: RequestRule)
  extends TypedBuilder[T]
  with RoutePrependable[RequestLineBuilder[T]]
  with UriConvertible {

  override def /:(prefix: TypedPath[HNil]): RequestLineBuilder[T] =
    copy(path = PathAnd(prefix.rule, path))

  def &[T1 <: HList](q: TypedQuery[T1])(implicit prep: Prepend[T1, T]): RequestLineBuilder[prep.Out] =
    RequestLineBuilder(path, AndRule(rules, q.rule))
}
