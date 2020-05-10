package org.http4s
package rho.bits

import shapeless.HList

import scala.annotation.implicitNotFound

@implicitNotFound("No ResultMatcher instance found for the return type of (${FU}).")
trait FuncResultMatch[F[_], T <: HList, -FU]

trait FuncResultMatchers[F[_]] {

  import shapeless._
  import shapeless.ops.function._

  implicit def constantResultMatch[R](implicit m: ResultMatcher[F, R]): FuncResultMatch[F, HNil, R] = new FuncResultMatch[F, HNil, R] {}

  implicit def functionResultMatch[T <: HList, TR <: HList, FU, R](implicit fp: FnToProduct.Aux[FU, TR => R], m: ResultMatcher[F, R]): FuncResultMatch[F, T, FU] = new FuncResultMatch[F, T, FU] {}
}
