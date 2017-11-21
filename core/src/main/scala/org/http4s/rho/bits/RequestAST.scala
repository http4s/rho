package org.http4s
package rho.bits

import cats.effect.IO
import shapeless.HList

/** Rules for extracting non-path related data from the `Request`
  *
  * This AST is subject to change.
  */
object RequestAST {

  /** Base trait of the AST */
  sealed trait RequestRule

  /** Captures data from the `Request` */
  case class CaptureRule[T](reader: Request[IO] => ResultResponse[T]) extends RequestRule

  /** Transform data from the `Request` */
  case class MapRule[T <: HList, R <: HList](rule: RequestRule, f: T => R) extends RequestRule

  /** Ignore any results obtained form the enclosed rules */
  case class IgnoreRule(rule: RequestRule) extends RequestRule

  /** Append the result as `b:::a:::HNil` */
  case class AndRule(fst: RequestRule, snd: RequestRule) extends RequestRule

  /** Attempt rule `fst` and attempt rule `alt` if `fst` fails */
  case class OrRule(fst: RequestRule, alt: RequestRule) extends RequestRule

  /** Append meta data to the tree */
  case class MetaRule(rule: RequestRule, meta: Metadata) extends RequestRule

  /** Empty rule */
  case object EmptyRule extends RequestRule
}
