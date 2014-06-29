package org.http4s.rho.bits

import org.http4s.{Header, HeaderKey}
import shapeless.ops.hlist.Prepend
import shapeless.{::, HList, HNil}

import scala.language.existentials

/** AST representing the Header operations of the DSL */
object HeaderAST {

  sealed trait HeaderRule[T <: HList] extends HeaderRuleSyntax[T] {

    final def or(v: HeaderRule[T]): HeaderRule[T] = Or(this, v)

    final def ||(v: HeaderRule[T]): HeaderRule[T] = or(v)

    final def and[T1 <: HList](v: HeaderRule[T1])(implicit prepend: Prepend[T, T1]): HeaderRule[prepend.Out] = And(this, v)

    final def &&[T1 <: HList](v: HeaderRule[T1])(implicit prepend: Prepend[T, T1]): HeaderRule[prepend.Out] = and(v)
  }

  /* this exists only to force method consistency on the Route and HeaderRules,
   not to play a role in the type tree */
  private[rho] trait HeaderRuleSyntax[T <: HList] {

    def or(v: HeaderRule[T]): HeaderRuleSyntax[T]

    def ||(v: HeaderRule[T]): HeaderRuleSyntax[T]

    def and[T1 <: HList](v: HeaderRule[T1])(implicit prepend: Prepend[T, T1]): HeaderRuleSyntax[prepend.Out]

    def &&[T1 <: HList](v: HeaderRule[T1])(implicit prepend: Prepend[T, T1]): HeaderRuleSyntax[prepend.Out]
  }

  ///////////////// Header and body AST ///////////////////////

  private[rho] case class And[T <: HList, T2 <: HList, T3 <: HList](a: HeaderRule[T2], b: HeaderRule[T3]) extends HeaderRule[T]

  private[rho] case class Or[T <: HList](a: HeaderRule[T], b: HeaderRule[T]) extends HeaderRule[T]

  private[rho] case class HeaderRequire[H <: HeaderKey.Extractable](key: H, f: H#HeaderT => Boolean) extends HeaderRule[HNil]

  private[rho] case class HeaderCapture[T <: Header](key: HeaderKey.Extractable) extends HeaderRule[T :: HNil]

  private[rho] case class HeaderMapper[H <: HeaderKey.Extractable, R](key: H, f: H#HeaderT => R) extends HeaderRule[R :: HNil]

  private[rho] case class QueryRule[T](name: String, p: QueryParser[T])(implicit val m: Manifest[T]) extends HeaderRule[T :: HNil]

  private[rho] object EmptyHeaderRule extends HeaderRule[HNil]

}