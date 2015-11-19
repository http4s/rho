package org.http4s
package rho

import org.http4s.rho.bits.AsRequestRule
import org.http4s.rho.bits.RequestRuleAST.{ RequestAnd, RequestRule }

import org.http4s.rho.bits.PathAST.{PathAnd, TypedPath, PathRule}
import shapeless.{HNil, HList}
import shapeless.ops.hlist.Prepend

case class RequestLineBuilder[T <: HList](path: PathRule, requestRules: RequestRule)
  extends TypedBuilder[T]
  with RoutePrependable[RequestLineBuilder[T]]
  with UriConvertible {

  override def /:(prefix: TypedPath[HNil]): RequestLineBuilder[T] =
    copy(path = PathAnd(prefix.rule, path))

  def &[QueryType, T1 <: HList](q: QueryType)(implicit ev: AsRequestRule[QueryType, T1], prep: Prepend[T1, T]): RequestLineBuilder[prep.Out] =
    RequestLineBuilder(path, RequestAnd(requestRules, ev(q).rule))
}
