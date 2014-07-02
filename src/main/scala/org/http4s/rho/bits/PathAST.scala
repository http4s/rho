package org.http4s
package rho.bits

import scala.language.existentials

import shapeless.ops.hlist.Prepend
import shapeless.{::, HNil, HList}

import scala.reflect.Manifest


/** Actual elements which build up the AST */

object PathAST {

  /** The root type of the parser AST */
  sealed trait PathRule[T <: HList] extends {
    /** These methods differ in their return type */
    def and[T2 <: HList](p2: PathRule[T2])(implicit prep: Prepend[T2, T]): PathRule[prep.Out] =
      PathAnd(this, p2)

    def &&[T2 <: HList](p2: PathRule[T2])(implicit prep: Prepend[T2, T]): PathRule[prep.Out] = and(p2)

    def or(p2: PathRule[T]): PathRule[T] = PathOr(this, p2)

    def ||(p2: PathRule[T]): PathRule[T] = or(p2)

    def /(s: String): PathRule[T] = PathAnd(this, PathMatch(s))

    def /(s: Symbol): PathRule[String :: T] = PathAnd(this, PathCapture(StringParser.strParser))

    def /[T2 <: HList](t: PathRule[T2])(implicit prep: Prepend[T2, T]): PathRule[prep.Out] =
      PathAnd(this, t)
  }

  case class PathAnd[T <: HList](p1: PathRule[_ <: HList], p2: PathRule[_ <: HList]) extends PathRule[T]

  case class PathOr[T <: HList](p1: PathRule[T], p2: PathRule[T]) extends PathRule[T]

  case class PathMatch(s: String) extends PathRule[HNil]

  case class PathCapture[T](parser: StringParser[T])
                           (implicit val m: Manifest[T]) extends PathRule[T :: HNil]

  // These don't fit the  operations of CombinablePathSyntax because they may
  // result in a change of the type of PathBulder
  // TODO: can I make this a case object?
  case class CaptureTail() extends PathRule[List[String] :: HNil]

  case object PathEmpty extends PathRule[HNil]

  case class MetaCons[T <: HList](path: PathRule[T], meta: Metadata) extends PathRule[T]
}
