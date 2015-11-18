package org.http4s
package rho.bits

import scala.language.existentials

import org.http4s.rho.UriConvertible

import shapeless.HList

object QueryAST {

  /** A TypedQuery acts as a shell to maintain type safety of the [[QueryRule]] construction and evaluation. */
  case class TypedQuery[T <: HList](rule: QueryRule) extends UriConvertible with QueryCombinators[T] {

    override protected def asQueryRule: TypedQuery[T] = this

    /**
     * Resolves names of query parameters to capture
     */
    def names: List[String] = collectNames(rule)

    private val uriTemplate =
      for (q <- UriConverter.createQuery(rule))
        yield UriTemplate(query = q)

    override def asUriTemplate(request: Request) =
      UriConvertible.respectPathInfo(uriTemplate, request)
  }

  object TypedQuery {
    implicit def typedQueryInstance[T <: HList]: AsTypedQuery[TypedQuery[T], T] = new AsTypedQuery[TypedQuery[T], T]{
      override def toQuery(t: TypedQuery[T]): TypedQuery[T] = t
    }
  }

  sealed trait QueryRule

  case class QueryCapture(reader: QueryReader[_]) extends QueryRule

  case class QueryAnd(a: QueryRule, b: QueryRule) extends QueryRule

  case class QueryOr(a: QueryRule, b: QueryRule) extends QueryRule

  case class MetaCons(query: QueryRule, meta: Metadata) extends QueryRule

  case object EmptyQuery extends QueryRule

  private def collectNames(rule: QueryRule): List[String] = {

    @scala.annotation.tailrec
    def go(r: List[QueryRule], acc: List[String]): List[String] = r match {
      case Nil => acc
      case MetaCons(query, _) :: rs => go(rs, acc)
      case QueryAnd(a, b) :: rs => go(a :: b :: rs, acc)
      case QueryCapture(reader) :: rs => go(rs, reader.parameters.map(_.name) ::: acc)
      case QueryOr(a, _) :: rs => go(a :: rs, acc)
      case EmptyQuery :: rs => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }

}
