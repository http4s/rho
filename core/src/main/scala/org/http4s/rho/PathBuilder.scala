package org.http4s
package rho

import org.http4s.rho.bits.QueryAST.{ TypedQuery, QueryRule, EmptyQuery }
import bits.HeaderAST._
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
  extends HeaderAppendable[T]
  with RouteExecutable[T]
  with UriConvertible {
  type Self = PathBuilder[T]

  override def headers: HeaderRule = EmptyHeaderRule

  override def query: QueryRule = EmptyQuery

  def +?[T1 <: HList](q: TypedQuery[T1])(implicit prep: Prepend[T1, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, q.rule)

  def /(t: CaptureTail): Router[List[String] :: T] = new Router(method, PathAnd(path, t), EmptyQuery, EmptyHeaderRule)

  def /(s: String): PathBuilder[T] = new PathBuilder(method, PathAnd(path, PathMatch(s)))


  def /(s: Symbol): PathBuilder[String :: T] = {
    val capture = PathCapture(s.name, StringParser.strParser, implicitly[TypeTag[String]])
    new PathBuilder(method, PathAnd(path, capture))
  }

  def /[T2 <: HList](t: TypedPath[T2])(implicit prep: Prepend[T2, T]): PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path, t.rule))

  def /[T2 <: HList](t: PathBuilder[T2])(implicit prep: Prepend[T2, T]): PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path, t.path))

  def /[T2 <: HList](t: RequestLineBuilder[T2])(implicit prep: Prepend[T2, T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, PathAnd(path, t.path), t.query)

  override def >>>[T1 <: HList](h2: TypedHeader[T1])(implicit prep: Prepend[T1, T]): Router[prep.Out] =
    validate(h2)

  def toAction: Router[T] = validate(TypedHeader[HNil](EmptyHeaderRule))

  def validate[T1 <: HList](h2: TypedHeader[T1])(implicit prep: Prepend[T1, T]): Router[prep.Out] =
    Router(method, path, EmptyQuery, h2.rule)

  def decoding[R](dec: EntityDecoder[R]): CodecRouter[T, R] = CodecRouter(toAction, dec)

  override def makeAction[F](f: F, hf: HListToFunc[T, F]): RhoAction[T, F] =
    new RhoAction(Router(method, path, EmptyQuery, EmptyHeaderRule), f, hf)

  override val asUriTemplate = for (p <- UriConverter.createPath(path)) yield UriTemplate(path = p)

}
