package org.http4s
package rho.bits

import scala.language.existentials

import org.http4s.rho.{QueryBuilder, RequestLineBuilder, UriConvertible}
import org.http4s.rho.bits.QueryAST.{QueryCapture, TypedQuery}

import shapeless.ops.hlist.Prepend
import shapeless.{ ::, HList }

import scala.reflect.runtime.universe.TypeTag

/** Actual elements which build up the AST */
object PathAST {

  final case class TypedPath[T <: HList](rule: PathRule) extends UriConvertible {
    /** These methods differ in their return type */
    def and[T2 <: HList](p2: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] =
      TypedPath(PathAnd(this.rule, p2.rule))

    def &&[T2 <: HList](p2: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] = and(p2)

    def or(p2: TypedPath[T]): TypedPath[T] = TypedPath(PathOr(this.rule, p2.rule))

    def ||(p2: TypedPath[T]): TypedPath[T] = or(p2)

    def /(s: String): TypedPath[T] = TypedPath(PathAnd(this.rule, PathMatch(s)))

    def /(s: Symbol): TypedPath[String :: T] = {
      val capture = PathCapture(s.name, StringParser.strParser, implicitly[TypeTag[String]])
      TypedPath(PathAnd(this.rule, capture))
    }

    def /[T2 <: HList](t: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] =
      TypedPath(PathAnd(this.rule, t.rule))

    def /[T2 <: HList](t: RequestLineBuilder[T2])(implicit prep: Prepend[T, T2]): RequestLineBuilder[prep.Out] =
      RequestLineBuilder(PathAnd(this.rule, t.path), t.query)

    def +?[QueryType, T1 <: HList](q: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prep: Prepend[T1, T]): RequestLineBuilder[prep.Out] =
      RequestLineBuilder(rule, ev(q).rule)

    private val uriTemplate =
      for (p <- UriConverter.createPath(rule))
        yield UriTemplate(path = p)

    override def asUriTemplate(request: Request) =
      UriConvertible.respectPathInfo(uriTemplate, request)
  }

  /** The root type of the parser AST */
  sealed trait PathRule
  sealed trait PathRoute extends PathRule
  sealed trait PathOperation extends PathRule

  case class PathAnd(p1: PathRule, p2: PathRule) extends PathRoute

  case class PathOr(p1: PathRule, p2: PathRule) extends PathRoute

  case class PathMatch(s: String) extends PathOperation

  case class PathCapture(name: String, parser: StringParser[_], m: TypeTag[_]) extends PathOperation

  case object CaptureTail extends PathOperation

  case class MetaCons(path: PathRule, meta: Metadata) extends PathOperation

}
