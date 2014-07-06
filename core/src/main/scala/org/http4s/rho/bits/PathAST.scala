package org.http4s
package rho.bits

import scala.language.existentials

import shapeless.ops.hlist.Prepend
import shapeless.{::, HNil, HList}

import scala.reflect.Manifest


/** Actual elements which build up the AST */

object PathAST {

  case class TypedPath[T <: HList](rule: PathRule) {
    /** These methods differ in their return type */
    def and[T2 <: HList](p2: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] =
      TypedPath(PathAnd(this.rule, p2.rule))

    def &&[T2 <: HList](p2: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] = and(p2)

    def or(p2: TypedPath[T]): TypedPath[T] = TypedPath(PathOr(this.rule, p2.rule))

    def ||(p2: TypedPath[T]): TypedPath[T] = or(p2)

    def /(s: String): TypedPath[T] = TypedPath(PathAnd(this.rule, PathMatch(s)))

    def /(s: Symbol): TypedPath[String :: T] =
      TypedPath(PathAnd(this.rule, PathCapture(StringParser.strParser, implicitly[Manifest[String]])))

    def /[T2 <: HList](t: TypedPath[T2])(implicit prep: Prepend[T2, T]): TypedPath[prep.Out] =
      TypedPath(PathAnd(this.rule, t.rule))
  }

  /** The root type of the parser AST */
  sealed trait PathRule

  case class PathAnd(p1: PathRule, p2: PathRule) extends PathRule

  case class PathOr(p1: PathRule, p2: PathRule) extends PathRule

  case class PathMatch(s: String) extends PathRule

  case class PathCapture(parser: StringParser[_], m: Manifest[_]) extends PathRule

  // These don't fit the  operations of CombinablePathSyntax because they may
  // result in a change of the type of PathBulder
  // TODO: can I make this a case object?
  case class CaptureTail() extends PathRule

  case object PathEmpty extends PathRule

  case class MetaCons(path: PathRule, meta: Metadata) extends PathRule
}
