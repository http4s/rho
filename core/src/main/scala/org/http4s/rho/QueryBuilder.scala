package org.http4s
package rho

import cats.Functor
import org.http4s.rho.bits.PathAST._
import org.http4s.rho.bits.RequestAST.{AndRule, EmptyRule, RequestRule}
import org.http4s.rho.bits.{HeaderAppendable, TypedHeader, TypedQuery}
import shapeless.ops.hlist.Prepend
import shapeless.{HList, HNil}

import scala.reflect.runtime.universe

/** Typed builder of query rules
  *
  * The [[QueryBuilder]] represents a builder for routes that already have a defined
  * method and path. It can accumulate query rules and mount decoders.
  *
  * @param method Request method to match.
  * @param path Path rules to execute.
  * @param rules Accumulated RequestRule's.
  * @tparam T The HList representation of the types the route expects to extract
  *           from a `Request`.
  */
case class QueryBuilder[F[_], T <: HList](method: Method, path: PathRule, rules: RequestRule[F])
    extends RouteExecutable[F, T]
    with Decodable[F, T, Nothing]
    with HeaderAppendable[F, T]
    with UriConvertible[F]
    with RoutePrependable[F, QueryBuilder[F, T]] {

  /** Capture a query rule
    *
    * @param query Query capture rule.
    * @tparam T1 types of elements captured by query.
    * @return a [[QueryBuilder]] with which to continue building the route.
    */
  def &[T1 <: HList](query: TypedQuery[F, T1])(implicit
      prep: Prepend[T1, T]): QueryBuilder[F, prep.Out] =
    QueryBuilder(method, path, AndRule(rules, query.rule))

  override def /:(prefix: TypedPath[F, HNil]): QueryBuilder[F, T] =
    new QueryBuilder[F, T](method, PathAnd(prefix.rule, path), rules)

  override type HeaderAppendResult[T <: HList] = Router[F, T]

  override def makeRoute(action: Action[F, T]): RhoRoute[F, T] =
    RhoRoute(Router(method, path, rules), action)

  override def >>>[T1 <: HList](rule: TypedHeader[F, T1])(implicit
      prep1: Prepend[T1, T]): Router[F, prep1.Out] =
    Router(method, path, AndRule(rules, rule.rule))

  /** Decode the body using the `EntityDecoder`
    *
    * Alias for the `^` operator.
    *
    * @param decoder `EntityDecoder` to utilize for decoding the body.
    * @tparam R2 type of the result.
    */
  override def decoding[R2 >: Nothing](decoder: EntityDecoder[F, R2])(implicit
      F: Functor[F],
      t: universe.TypeTag[R2]): CodecRouter[F, T, R2] =
    CodecRouter(>>>(TypedHeader[F, HNil](EmptyRule[F]())), decoder)
}
