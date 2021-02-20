package org.http4s
package rho

import org.http4s.rho.bits.PathAST.{PathAnd, PathRule, TypedPath}
import org.http4s.rho.bits.RequestAST.{AndRule, RequestRule}
import org.http4s.rho.bits.TypedQuery
import shapeless.{HList, HNil}
import shapeless.ops.hlist.Prepend

/** DSL construct for building query rules without a known `Method`
  *
  * The [[RequestLineBuilder]] doesn't know about header rules or the method.
  *
  * @tparam T The `HList` representation of the values to be extracted from the `Request`.
  */
final case class RequestLineBuilder[F[_], T <: HList](path: PathRule, rules: RequestRule[F])
    extends TypedBuilder[F, T]
    with RoutePrependable[F, RequestLineBuilder[F, T]]
    with UriConvertible[F] {

  /** Prepend the prefix to the path rules
    *
    * @param prefix The non-capturing prefix to prepend.
    * @return A [[RequestLineBuilder]] with the prefix prepended to the path rules.
    */
  override def /:(prefix: TypedPath[F, HNil]): RequestLineBuilder[F, T] =
    copy(path = PathAnd(prefix.rule, path))

  /** Capture a query rule
    *
    * @param query Query capture rule.
    * @tparam T1 The types of elements captured by query.
    * @return A [[QueryBuilder]] with which to continue building the route.
    */
  def &[T1 <: HList](query: TypedQuery[F, T1])(implicit
      prep: Prepend[T1, T]): RequestLineBuilder[F, prep.Out] =
    RequestLineBuilder(path, AndRule(rules, query.rule))
}
