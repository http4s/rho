package org.http4s
package rho

import bits.PathAST._
import bits._
import org.http4s.rho.bits.RequestAST.{RequestRule, EmptyRule}

import shapeless.{ HNil, ::, HList }
import shapeless.ops.hlist.Prepend

import scala.reflect.runtime.universe.TypeTag

/**
 * The goal of a PathBuilder is to allow the composition of what is typically on the status
 * line of a HTTP request. That includes the request method, path, and query params.
 */

/** Fully functional path building */
final class PathBuilder[T <: HList](val method: Method, val path: PathRule)
  extends RouteExecutable[T]
  with Decodable[T, Nothing]
  with HeaderAppendable[T]
  with RoutePrependable[PathBuilder[T]]
  with UriConvertible
{
  type HeaderAppendResult[T <: HList] = Router[T]

  override val rules: RequestRule = EmptyRule

  /** Capture a query rule
    *
    * @param query Query capture rule
    * @tparam T1 types of elements captured by query
    * @return a [[QueryBuilder]] with which to continue building the route
    */
  def +?[T1 <: HList](query: TypedQuery[T1])(implicit prep: Prepend[T1, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, query.rule)

  /** Capture the remaining elements in the path
    *
    * @param tail
    * @return a [[Router]]
    */
  def /(tail: CaptureTail.type): Router[List[String] :: T] = new Router(method, PathAnd(path, tail), EmptyRule)

  /** Match against a `String`
    *
    * @param segment `String` to match against.
    * @return a new [[PathBuilder]] that will match against the provided `String`.
    */
  def /(segment: String): PathBuilder[T] = {
    val newPath = segment.split("/").foldLeft(path)((p, s) => PathAnd(p, PathMatch(s)))
    new PathBuilder[T](method, newPath)
  }

  /** Capture a `String` from the path
    *
    * @param symbol `Symbol` representing the name of the segment to capture.
    * @return a new [[PathBuilder]] that will capture a uri segment.
    */
  def /(symbol: Symbol): PathBuilder[String :: T] = {
    val capture = PathCapture(symbol.name, None, StringParser.strParser, implicitly[TypeTag[String]])
    new PathBuilder(method, PathAnd(path, capture))
  }

  /** Append the path rules to the [[PathBuilder]]
    *
    * @param rules [[TypedPath]] rules to append to the path capture rules.
    * @return a new [[PathBuilder]] that will execute the appended rules.
    */
  def /[T2 <: HList](rules: TypedPath[T2])(implicit prep: Prepend[T2, T]): PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path, rules.rule))

  /** Append the path and rules
    *
    * @param builder [[RequestLineBuilder]] rules to append to the path capture rules.
    * @return a new [[QueryBuilder]] that will execute the appended rules.
    */
  def /[T2 <: HList](builder: RequestLineBuilder[T2])(implicit prep: Prepend[T2, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, PathAnd(path, builder.path), builder.rules)

  override def /:(prefix: TypedPath[HNil]): PathBuilder[T] =
    new PathBuilder(method, PathAnd(prefix.rule, path))

  override def >>>[T1 <: HList](h2: TypedHeader[T1])(implicit prep: Prepend[T1, T]): Router[prep.Out] =
    Router(method, path, h2.rule)

  override def decoding[R](decoder: EntityDecoder[R])(implicit t: TypeTag[R]): CodecRouter[T, R] =
    CodecRouter(>>>(TypedHeader[HNil](EmptyRule)), decoder)

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(Router(method, path, EmptyRule), action)
}
