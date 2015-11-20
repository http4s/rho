package org.http4s
package rho.bits

import org.http4s.rho.bits.RequestAST.{AndRule, OrRule, RequestRule}
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList}

/** AST representing the Header operations of the DSL */
object HeaderAST {

  final case class TypedHeader[T <: HList](rule: RequestRule) {

    def or(v: TypedHeader[T]): TypedHeader[T] = TypedHeader(OrRule(this.rule, v.rule))

    def ||(v: TypedHeader[T]): TypedHeader[T] = or(v)

    def and[T1 <: HList](v: TypedHeader[T1])(implicit prepend: Prepend[T1, T]): TypedHeader[prepend.Out] =
      TypedHeader(AndRule(this.rule, v.rule))

    def &&[T1 <: HList](v: TypedHeader[T1])(implicit prepend: Prepend[T1, T]): TypedHeader[prepend.Out] = and(v)
  }
}