package org.http4s
package rho.bits

/** Rules for extracting non-path related data from the `Request``
  *
  * This AST is subject to change.
  */
object RequestAST {

  sealed trait RequestRule

  /** Captures data from the `Request` */
  case class CaptureRule[T](reader: Request => ResultResponse[T]) extends RequestRule

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
