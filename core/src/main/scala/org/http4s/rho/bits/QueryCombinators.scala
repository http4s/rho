package org.http4s.rho.bits

import org.http4s.rho.bits.QueryAST.{QueryAnd, QueryOr, TypedQuery}
import shapeless.ops.hlist.Prepend
import shapeless.{HList, HNil, ::}


trait QueryCombinators[T <: HList] {

  protected def asQueryRule: TypedQuery[T]

  final def or[QueryType](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T]): TypedQuery[T] =
    TypedQuery(QueryOr(asQueryRule.rule, ev(v).rule))

  final def ||[QueryType](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T]): TypedQuery[T] =
    or(v)

  final def and[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prepend: Prepend[T1, T]): TypedQuery[prepend.Out] =
    TypedQuery(QueryAnd(asQueryRule.rule, ev(v).rule))

  final def &&[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prepend: Prepend[T1, T]): TypedQuery[prepend.Out] =
    and(v)

  final def &[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prepend: Prepend[T1, T]): TypedQuery[prepend.Out] =
    and(v)
}

trait AsTypedQuery[T, R <: HList] {
  def toQuery(t: T): TypedQuery[R]

  final def apply(t: T): TypedQuery[R] = toQuery(t)
}

object AsTypedQuery {

}