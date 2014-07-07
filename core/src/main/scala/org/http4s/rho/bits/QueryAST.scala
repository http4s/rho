package org.http4s
package rho.bits

import shapeless.{HNil, ::, HList}
import shapeless.ops.hlist.Prepend


object QueryAST {

  case class TypedQuery[T <: HList](rule: QueryRule) {
    final def or(v: TypedQuery[T]): TypedQuery[T] = TypedQuery(QueryOr(this.rule, v.rule))

    final def ||(v: TypedQuery[T]): TypedQuery[T] = or(v)

    final def and[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T, T1]): TypedQuery[prepend.Out] =
      TypedQuery(QueryAnd(this.rule, v.rule))

    final def &&[T1 <: HList](v: TypedQuery[T1])(implicit prepend: Prepend[T, T1]): TypedQuery[prepend.Out] = and(v)
  }

  sealed trait QueryRule

  case class QueryCapture[T](name: String, p: QueryParser[T], default: Option[T], m: Manifest[T]) extends QueryRule

  case class QueryAnd(a: QueryRule, b: QueryRule) extends QueryRule

  case class QueryOr(a: QueryRule, b: QueryRule) extends QueryRule

  case class MetaCons(query: QueryRule, meta: Metadata) extends QueryRule

  case object EmptyQuery extends QueryRule
}
