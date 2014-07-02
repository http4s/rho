package org.http4s
package rho.bits

import shapeless.{HNil, ::, HList}
import shapeless.ops.hlist.Prepend


object QueryAST {

  sealed trait QueryRule[T <: HList] {
    final def or(v: QueryRule[T]): QueryRule[T] = QueryOr(this, v)

    final def ||(v: QueryRule[T]): QueryRule[T] = or(v)

    final def and[T1 <: HList](v: QueryRule[T1])(implicit prepend: Prepend[T, T1]): QueryRule[prepend.Out] = QueryAnd(this, v)

    final def &&[T1 <: HList](v: QueryRule[T1])(implicit prepend: Prepend[T, T1]): QueryRule[prepend.Out] = and(v)
  }

  case class QueryCapture[T](name: String, p: QueryParser[T])(implicit val m: Manifest[T]) extends QueryRule[T :: HNil]

  case class QueryAnd[T <: HList, T2 <: HList, T3 <: HList](a: QueryRule[T2], b: QueryRule[T3]) extends QueryRule[T]

  case class QueryOr[T <: HList](a: QueryRule[T], b: QueryRule[T]) extends QueryRule[T]

  case object EmptyQuery extends QueryRule[HNil]
}
