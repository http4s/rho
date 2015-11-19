package org.http4s
package rho

import org.http4s.rho.bits.RequestRuleAST.{ TypedRequestRule, RequestRule, EmptyRule }
import bits.PathAST._
import bits._

import shapeless.{ HNil, ::, HList }
import shapeless.ops.hlist.Prepend

import scala.reflect.runtime.universe.TypeTag

/**
 * The goal of a PathBuilder is to allow the composition of what is typically on the status line
 * of a HTTP request. That includes the request method, path, and query params.
 */

/** Fully functional path building */
final class PathBuilder[T <: HList](val method: Method, val path: PathRule)
  extends RouteExecutable[T]
  with Decodable[T, Nothing]
  with UriConvertible
  with RoutePrependable[PathBuilder[T]]
{
  type HeaderAppendResult[T <: HList] = Router[T]

  override def requestRules: RequestRule = EmptyRule

  final def +?[QueryType, T1 <: HList](q: QueryType)(implicit ev: AsRequestRule[QueryType, T1], prep: Prepend[T1, T]): Router[prep.Out] =
    Router(method, path, ev(q).rule)

  def /(t: CaptureTail.type): Router[List[String] :: T] = Router(method, PathAnd(path, t), EmptyRule)

  def /(s: String): PathBuilder[T] = {
    val newPath = s.split("/").foldLeft(path)((p,s) => PathAnd(p, PathMatch(s)))
    new PathBuilder[T](method, newPath)
  }

  def /(s: Symbol): PathBuilder[String :: T] = {
    val capture = PathCapture(s.name, StringParser.strParser, implicitly[TypeTag[String]])
    new PathBuilder(method, PathAnd(path, capture))
  }

  def /[T2 <: HList](t: TypedPath[T2])(implicit prep: Prepend[T2, T]): PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path, t.rule))

  def /[T2 <: HList](t: PathBuilder[T2])(implicit prep: Prepend[T2, T]): PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path, t.path))

  def /[T2 <: HList](t: RequestLineBuilder[T2])(implicit prep: Prepend[T2, T]): Router[prep.Out] =
    Router(method, PathAnd(path, t.path), t.requestRules)

  override def /:(prefix: TypedPath[HNil]): PathBuilder[T] =
    new PathBuilder(method, PathAnd(prefix.rule, path))

  def toAction: Router[T] = >>>[TypedRequestRule[HNil], HNil](TypedRequestRule[HNil](EmptyRule))

  def >>>[O, T1 <: HList](h2: O)(implicit ev: AsRequestRule[O, T1], prep: Prepend[T1, T]): Router[prep.Out] =
    Router(method, path, ev(h2).rule)

  override def decoding[R](decoder: EntityDecoder[R])(implicit t: TypeTag[R]): CodecRouter[T, R] =
    CodecRouter(toAction, decoder)

  override def makeRoute(action: Action[T]): RhoRoute[T] = RhoRoute(Router(method, path, EmptyRule), action)
}
