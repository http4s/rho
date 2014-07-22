package org.http4s
package rho.bits

import shapeless.ops.hlist.Prepend
import shapeless.{::, HList}

/** AST representing the Header operations of the DSL */
object HeaderAST {

  final case class TypedHeader[T <: HList](rule: HeaderRule) {

    def or(v: TypedHeader[T]): TypedHeader[T] = TypedHeader(HeaderOr(this.rule, v.rule))

    def ||(v: TypedHeader[T]): TypedHeader[T] = or(v)

    def and[T1 <: HList](v: TypedHeader[T1])(implicit prepend: Prepend[T, T1]): TypedHeader[prepend.Out] =
      TypedHeader(HeaderAnd(this.rule, v.rule))

    def &&[T1 <: HList](v: TypedHeader[T1])(implicit prepend: Prepend[T, T1]): TypedHeader[prepend.Out] = and(v)
  }

  ///////////////// Header and body AST ///////////////////////

  sealed trait HeaderRule

  case class HeaderRequire[T <: HeaderKey.Extractable](key: T, f: T#HeaderT => Boolean) extends HeaderRule

  case class HeaderMapper[T <: HeaderKey.Extractable, R](key: T, f: T#HeaderT => R) extends HeaderRule

  case class HeaderCapture(key: HeaderKey.Extractable) extends HeaderRule

  case class HeaderAnd(a: HeaderRule, b: HeaderRule) extends HeaderRule

  case class HeaderOr(a: HeaderRule, b: HeaderRule) extends HeaderRule

  case class MetaCons(a: HeaderRule, meta: Metadata) extends HeaderRule

  object EmptyHeaderRule extends HeaderRule

}