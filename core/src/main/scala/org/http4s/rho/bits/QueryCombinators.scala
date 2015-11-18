package org.http4s.rho.bits

import org.http4s.rho.bits.QueryAST.{QueryAnd, QueryOr, TypedQuery}
import shapeless.ops.hlist.Prepend
import shapeless.{HList, HNil, ::}


trait QueryCombinators[T <: HList] {

  protected def asRule: TypedQuery[T]

  final def or[QueryType](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T]): TypedQuery[T] =
    TypedQuery(QueryOr(asRule.rule, ev(v).rule))

  final def ||[QueryType](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T]): TypedQuery[T] =
    or(v)

  final def and[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsTypedQuery[QueryType, T1], prepend: Prepend[T1, T]): TypedQuery[prepend.Out] =
    TypedQuery(QueryAnd(asRule.rule, ev(v).rule))

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

  implicit def typedQueryInstance[T <: HList]: AsTypedQuery[TypedQuery[T], T] = new AsTypedQuery[TypedQuery[T], T]{
    override def toQuery(t: TypedQuery[T]): TypedQuery[T] = t
  }

  implicit def queryReaderInstance[T]: AsTypedQuery[QueryReader[T], T::HNil] = new AsTypedQuery[QueryReader[T], T::HNil] {
    override def toQuery(t: QueryReader[T]): TypedQuery[::[T, HNil]] = t.asRule
  }

}