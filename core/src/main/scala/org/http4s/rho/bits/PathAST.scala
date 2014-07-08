package org.http4s
package rho.bits

import scala.language.existentials

import shapeless.ops.hlist.Prepend
import shapeless.{ ::, HList }

import scala.reflect.runtime.universe.TypeTag

import org.http4s.UriTemplate
import org.http4s.UriTemplate._
import org.http4s.rho.UriConvertible

import shapeless.ops.hlist.Prepend
import shapeless.{ ::, HNil, HList }

/** Actual elements which build up the AST */

object PathAST {

  case class TypedPath[T <: HList](rule: PathRule) extends UriConvertible {
    /** These methods differ in their return type */
    def and[T2 <: HList](p2: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] =
      TypedPath(PathAnd(this.rule, p2.rule))

    def &&[T2 <: HList](p2: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] = and(p2)

    def or(p2: TypedPath[T]): TypedPath[T] = TypedPath(PathOr(this.rule, p2.rule))

    def ||(p2: TypedPath[T]): TypedPath[T] = or(p2)

    def /(s: String): TypedPath[T] = TypedPath(PathAnd(this.rule, PathMatch(s)))

    def /(s: Symbol): TypedPath[String :: T] = {
      val capture = PathCapture(StringParser.strParser, implicitly[TypeTag[String]])
      TypedPath(PathAnd(this.rule, PathAST.MetaCons(capture, TextMeta(s.name, s"Param name: ${s.name}"))))
    }

    def /[T2 <: HList](t: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] =
      TypedPath(PathAnd(this.rule, t.rule))

    override def asUriTemplate = for (p <- UriConverter.createPath(rule)) yield UriTemplate(path = p)
  }

  /** The root type of the parser AST */
  sealed trait PathRule

  case class PathAnd(p1: PathRule, p2: PathRule) extends PathRule

  case class PathOr(p1: PathRule, p2: PathRule) extends PathRule

  case class PathMatch(s: String) extends PathRule

  case class PathCapture(parser: StringParser[_], m: TypeTag[_]) extends PathRule

  // These don't fit the  operations of CombinablePathSyntax because they may
  // result in a change of the type of PathBulder
  // TODO: can I make this a case object?
  case class CaptureTail() extends PathRule

  case object PathEmpty extends PathRule

  case class MetaCons(path: PathRule, meta: Metadata) extends PathRule

}
