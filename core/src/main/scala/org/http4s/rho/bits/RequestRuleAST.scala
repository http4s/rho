package org.http4s
package rho.bits

import org.http4s.rho.{RequestReader, UriConvertible}
import RequestReader.QueryCaptureParams

import scala.language.existentials

import org.http4s.rho.UriConvertible

import shapeless.HList

object RequestRuleAST {

  /** A TypedQuery acts as a shell to maintain type safety of the [[RequestRule]] construction and evaluation. */
  case class TypedRequestRule[T <: HList](rule: RequestRule) extends UriConvertible with RequestExtractorCombinators[T] {

    override protected def asQueryRule: TypedRequestRule[T] = this

    /**
     * Resolves names of query parameters to capture
     */
    def queryNames: List[String] = collectQueryNames(rule)

    private val uriTemplate =
      for (q <- UriConverter.createQuery(rule))
        yield UriTemplate(query = q)

    override def asUriTemplate(request: Request) =
      UriConvertible.respectPathInfo(uriTemplate, request)
  }

  object TypedRequestRule {
    implicit def typedRequestRuleInstance[T <: HList]: AsRequestRule[TypedRequestRule[T], T] = new AsRequestRule[TypedRequestRule[T], T]{
      override def toRule(t: TypedRequestRule[T]): TypedRequestRule[T] = t
    }
  }

  sealed trait RequestRule

  case class RequestCapture(reader: RequestReader[_]) extends RequestRule

  case class RequestAnd(a: RequestRule, b: RequestRule) extends RequestRule

  case class RequestOr(a: RequestRule, b: RequestRule) extends RequestRule

  case class MetaCons(query: RequestRule, meta: Metadata) extends RequestRule

  case object EmptyRule extends RequestRule

  private def collectQueryNames(rule: RequestRule): List[String] = {

    @scala.annotation.tailrec
    def go(r: List[RequestRule], acc: List[String]): List[String] = r match {
      case Nil => acc
      case MetaCons(query, _) :: rs => go(rs, acc)
      case RequestAnd(a, b) :: rs => go(a :: b :: rs, acc)
      case RequestCapture(reader) :: rs => go(rs, reader.queryNames ::: acc)
      case RequestOr(a, _) :: rs => go(a :: rs, acc)
      case EmptyRule :: rs => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }

}
