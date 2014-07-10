package org.http4s
package rho.bits

import org.http4s.rho.UriConvertible

import scala.reflect.runtime.universe.TypeTag
import shapeless.HList
import shapeless.ops.hlist.Prepend

object QueryAST {

  case class TypedQuery[T <: HList](rule: QueryRule) extends UriConvertible {
    final def or(v: TypedQuery[T]): TypedQuery[T] = TypedQuery(QueryOr(this.rule, v.rule))

    final def ||(v: TypedQuery[T]): TypedQuery[T] = or(v)

    final def and[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T, T1]): TypedQuery[prepend.Out] =
      TypedQuery(QueryAnd(this.rule, v.rule))

    final def &&[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T, T1]): TypedQuery[prepend.Out] = and(v)

    /**
     * Resolves names of query parameters to capture
     */
    lazy val names: List[String] = collectNames(rule)

    override def asUriTemplate = for (q <- UriConverter.createQuery(rule)) yield UriTemplate(query = Some(q))
  }

  sealed trait QueryRule

  case class QueryCapture[T](name: String, p: QueryParser[T], default: Option[T], m: TypeTag[T]) extends QueryRule

  case class QueryAnd(a: QueryRule, b: QueryRule) extends QueryRule

  case class QueryOr(a: QueryRule, b: QueryRule) extends QueryRule

  case class MetaCons(query: QueryRule, meta: Metadata) extends QueryRule

  case object EmptyQuery extends QueryRule

  private def collectNames(rule: QueryRule): List[String] = {
    @scala.annotation.tailrec
    def go(r: List[QueryRule], acc: List[String]): List[String] = r match {
      case Nil => acc
      case MetaCons(query, meta) :: rs => go(rs, acc)
      case QueryAnd(a, b) :: rs => go(a :: b :: rs, acc)
      case QueryCapture(name, p, default, m) :: rs => go(rs, name :: acc)
      case QueryOr(a, b) :: rs => go(a :: rs, acc)
      case EmptyQuery :: rs => go(rs, acc)
    }
    go(List(rule), Nil).reverse
  }

}
