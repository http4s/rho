package org.http4s
package rho.bits

import scala.language.existentials

import org.http4s.rho.UriConvertible

import shapeless.{HNil, HList, :: => :#:}
import shapeless.ops.hlist.Prepend

import scalaz.Applicative

object QueryAST {

  /** A TypedQuery acts as a shell to maintain type safety of the [[QueryRule]] construction and evaluation. */
  case class TypedQuery[T <: HList](rule: QueryRule) extends UriConvertible with QueryCombinators[T] {

    override protected def asRule: TypedQuery[T] = this

    /**
     * Resolves names of query parameters to capture
     */
    val names: List[String] = collectNames(rule)

    private val uriTemplate =
      for (q <- UriConverter.createQuery(rule))
        yield UriTemplate(query = q)

    override def asUriTemplate(request: Request) =
      UriConvertible.respectPathInfo(uriTemplate, request)
  }

  object TypedQuery {
//    import shapeless.{HNil, ::}
//    implicit def queryParamKeyLike[T]: QueryParamKeyLike[TypedQuery[T::HNil]] = new QueryParamKeyLike[TypedQuery[T::HNil]] {
//      override def getKey(t: TypedQuery[T::HNil]): QueryParameterKey =
//        getKey(t.rule).getOrElse(sys.error("Empty Query doesn't have a key name"))
//
//      private def getKey(rule: QueryRule): Option[QueryParameterKey] = rule match {
//        case QueryCapture(n,_,_,_) => Some(QueryParameterKey(n))
//        case QueryOr(a, b)         => getKey(a) orElse getKey(b)
//        case MetaCons(r,_)         => getKey(r)
//        case QueryAnd(a, b)        => getKey(a) orElse getKey(b) // shouldn't get here
//        case EmptyQuery            => None                       // shouldn't get here
//      }
//    }
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
      case QueryCapture(reader) :: rs => go(rs, reader.parameters.map(_.name).toList ::: acc)
      case QueryOr(a, _) :: rs => go(a :: rs, acc)
      case EmptyQuery :: rs => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }

}
