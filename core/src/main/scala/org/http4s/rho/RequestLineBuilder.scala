package org.http4s
package rho

import org.http4s.rho.bits.PathAST.{PathAnd, TypedPath, PathRule}
import org.http4s.rho.bits.RequestAST.{AndRule, RequestRule}
import org.http4s.rho.bits.TypedQuery
import shapeless.{HNil, HList}
import shapeless.ops.hlist.Prepend

/** DSL construct for building query rules without a known `Method`
  *
  * The [[RequestLineBuilder]] doesn't know about header rules or the method.
  *
  * @tparam T The `HList` representation of the values to be extracted from the `Request`.
  */
final case class RequestLineBuilder[T <: HList](path: PathRule, rules: RequestRule)
  extends TypedBuilder[T]
  with RoutePrependable[RequestLineBuilder[T]]
  with UriConvertible {

  /** Prepend the prefix to the path rules
    *
    * @param prefix The non-capturing prefix to prepend.
    * @return A [[RequestLineBuilder]] with the prefix prepended to the path rules.
    */
  override def /:(prefix: TypedPath[HNil]): RequestLineBuilder[T] =
    copy(path = PathAnd(prefix.rule, path))

  /** Capture a query rule
    *
    * @param query Query capture rule.
    * @tparam T1 The types of elements captured by query.
    * @return A [[QueryBuilder]] with which to continue building the route.
    */
  def &[T1 <: HList](query: TypedQuery[T1])(implicit prep: Prepend[T1, T]): RequestLineBuilder[prep.Out] =
    RequestLineBuilder(path, AndRule(rules, query.rule))
}
