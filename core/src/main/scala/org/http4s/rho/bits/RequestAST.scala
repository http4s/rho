package org.http4s
package rho.bits

object RequestAST {

  sealed trait RequestRule

  case class CaptureRule[T](reader: Request => ResultResponse[T]) extends RequestRule

  case class IgnoreRule(rule: RequestRule) extends RequestRule

  case class AndRule(a: RequestRule, b: RequestRule) extends RequestRule

  case class OrRule(a: RequestRule, b: RequestRule) extends RequestRule

  case class MetaRule(query: RequestRule, meta: Metadata) extends RequestRule

  case object EmptyRule extends RequestRule
}
