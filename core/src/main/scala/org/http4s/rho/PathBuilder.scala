package org.http4s
package rho

import bits.PathAST._
import bits._
import cats.Functor
import org.http4s.rho.bits.RequestAST.{EmptyRule, RequestRule}
import shapeless.{::, HList, HNil}
import shapeless.ops.hlist.Prepend

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.TypeTag

/**
 * The goal of a PathBuilder is to allow the composition of what is typically on the status
 * line of a HTTP request. That includes the request method, path, and query params.
 */

/** Fully functional path building */
final class PathBuilder[F[_], T <: HList](val method: Method, val path: PathRule)
  extends RouteExecutable[F, T]
  with Decodable[F, T, Nothing]
  with HeaderAppendable[F, T]
  with RoutePrependable[F, PathBuilder[F, T]]
  with UriConvertible[F]
{
  type HeaderAppendResult[T <: HList] = Router[F, T]

  override val rules: RequestRule[F] = EmptyRule[F]()

  /** Capture a query rule
    *
    * @param query Query capture rule
    * @tparam T1 types of elements captured by query
    * @return a [[QueryBuilder]] with which to continue building the route
    */
  def +?[T1 <: HList](query: TypedQuery[F, T1])(implicit prep: Prepend[T1, T]): QueryBuilder[F, prep.Out] =
    QueryBuilder(method, path, query.rule)

  /** Capture the remaining elements in the path
    *
    * @param tail
    * @return a [[Router]]
    */
  def /(tail: CaptureTail.type): Router[F, List[String] :: T] = new Router(method, PathAnd(path, tail), EmptyRule[F]())

  /** Match against a `String`
    *
    * @param segment `String` to match against.
    * @return a new [[PathBuilder]] that will match against the provided `String`.
    */
  def /(segment: String): PathBuilder[F, T] = {
    val newPath = segment.split("/").foldLeft(path)((p, s) => PathAnd(p, PathMatch(s)))
    new PathBuilder[F, T](method, newPath)
  }

  /** Capture a `String` from the path
    *
    * @param symbol `Symbol` representing the name of the segment to capture.
    * @return a new [[PathBuilder]] that will capture a uri segment.
    */
  def /(symbol: Symbol): PathBuilder[F, String :: T] = {
    val capture = PathCapture(symbol.name, None, StringParser.strParser[F], implicitly[TypeTag[String]])
    new PathBuilder(method, PathAnd(path, capture))
  }

  /** Append the path rules to the [[PathBuilder]]
    *
    * @param rules [[TypedPath]] rules to append to the path capture rules.
    * @return a new [[PathBuilder]] that will execute the appended rules.
    */
  def /[T2 <: HList](rules: TypedPath[F, T2])(implicit prep: Prepend[T2, T]): PathBuilder[F, prep.Out] =
    new PathBuilder(method, PathAnd(path, rules.rule))

  /** Append the path and rules
    *
    * @param builder [[RequestLineBuilder]] rules to append to the path capture rules.
    * @return a new [[QueryBuilder]] that will execute the appended rules.
    */
  def /[T2 <: HList](builder: RequestLineBuilder[F, T2])(implicit prep: Prepend[T2, T]): QueryBuilder[F, prep.Out] =
    QueryBuilder(method, PathAnd(path, builder.path), builder.rules)

  override def /:(prefix: TypedPath[F, HNil]): PathBuilder[F, T] =
    new PathBuilder(method, PathAnd(prefix.rule, path))

  override def >>>[T1 <: HList](h2: TypedHeader[F, T1])(implicit prep: Prepend[T1, T]): Router[F, prep.Out] =
    Router(method, path, h2.rule)

  override def decoding[R](decoder: EntityDecoder[F, R])(implicit F: Functor[F], t: TypeTag[R]): CodecRouter[F, T, R] =
    CodecRouter(>>>(TypedHeader[F, HNil](EmptyRule[F]())), decoder)

  override def makeRoute(action: Action[F, T]): RhoRoute[F, T] = RhoRoute(Router(method, path, EmptyRule[F]()), action)
}
