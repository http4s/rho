package org.http4s.rho.bits

import org.http4s.rho.bits.RequestRuleAST.{RequestAnd, RequestOr, TypedRequestRule}
import shapeless.ops.hlist.Prepend
import shapeless.{HList, HNil, ::}


trait RequestExtractorCombinators[T <: HList] {

  protected def asQueryRule: TypedRequestRule[T]

  final def or[QueryType](v: QueryType)(implicit ev: AsRequestRule[QueryType, T]): TypedRequestRule[T] =
    TypedRequestRule(RequestOr(asQueryRule.rule, ev(v).rule))

  final def ||[QueryType](v: QueryType)(implicit ev: AsRequestRule[QueryType, T]): TypedRequestRule[T] =
    or(v)

  final def and[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsRequestRule[QueryType, T1], prepend: Prepend[T1, T]): TypedRequestRule[prepend.Out] =
    TypedRequestRule(RequestAnd(asQueryRule.rule, ev(v).rule))

  final def &&[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsRequestRule[QueryType, T1], prepend: Prepend[T1, T]): TypedRequestRule[prepend.Out] =
    and(v)

  final def &[QueryType, T1 <: HList](v: QueryType)(implicit ev: AsRequestRule[QueryType, T1], prepend: Prepend[T1, T]): TypedRequestRule[prepend.Out] =
    and(v)
}

trait AsRequestRule[T, R <: HList] {
  def toRule(t: T): TypedRequestRule[R]

  final def apply(t: T): TypedRequestRule[R] = toRule(t)
}
