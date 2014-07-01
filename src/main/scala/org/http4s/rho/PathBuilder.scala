package org.http4s.rho

import scala.language.existentials

import org.http4s.rho.bits.QueryAST.{QueryRule, EmptyQuery}
import bits.PathAST._
import bits.HeaderAST._

import shapeless.{::, HList}
import shapeless.ops.hlist.Prepend
import org.http4s.Method
import org.http4s.rho.bits._

/**
 * Created by Bryce Anderson on 4/28/14.
 *
 * The goal of a PathBuilder is to allow the composition of what is typically on the status line
 * of a HTTP request. That includes the request method, path, and query params.
 */

/** Fully functional path building */
final class PathBuilder[T <: HList](val method: Method, private[rho] val path: PathRule[T])
                  extends PathBuilderBase[T] with HeaderAppendable[T] with MetaDataSyntax {

  type Self = PathBuilder[T]

  def +?[T1 <: HList](q: QueryRule[T1])(implicit prep: Prepend[T1,T]): QueryBuilder[prep.Out] =
    QueryBuilder(method, path, q)

  def /(t: CaptureTail) : Router[List[String]::T] = new Router(method, PathAnd(path,t), EmptyQuery, EmptyHeaderRule)

  def /(s: String): PathBuilder[T] = new PathBuilder(method, PathAnd(path,PathMatch(s)))

  def /(s: Symbol): PathBuilder[String::T] = new PathBuilder(method, PathAnd(path,PathCapture(StringParser.strParser)))

  def /[T2 <: HList](t: CombinablePathRule[T2])(implicit prep: Prepend[T2, T]) : PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path,t))

  def /[T2 <: HList](t: PathBuilder[T2])(implicit prep: Prepend[T2, T]) : PathBuilder[prep.Out] =
    new PathBuilder(method, PathAnd(path, t.path))

  override protected def addMetaData(data: MetaData): PathBuilder[T] = new PathBuilder[T](method, PathAnd(path, data))
}

////////////////// AST representation of operations supported on the path ///////////////////


sealed trait PathBuilderBase[T <: HList] extends RouteExecutable[T] with HeaderAppendable[T] {
  def method: Method
  private[rho] def path: PathRule[T]

  override private[rho] def validators: HeaderRule[_ <: HList] = EmptyHeaderRule

  final def toAction: Router[T] = validate(EmptyHeaderRule)

  final def validate[T1 <: HList](h2: HeaderRule[T1])(implicit prep: Prepend[T1,T]): Router[prep.Out] =
    Router(method, path, EmptyQuery, h2)

  override final def >>>[T1 <: HList](h2: HeaderRule[T1])(implicit prep: Prepend[T1,T]): Router[prep.Out] = validate(h2)

  final def decoding[R](dec: Decoder[R]): CodecRouter[T, R] = CodecRouter(toAction, dec)

  final def makeAction[F, O](f: F, hf: HListToFunc[T,O,F]): RhoAction[T, F, O] =
    new RhoAction(Router(method, path, EmptyQuery, EmptyHeaderRule), f, hf)
}
