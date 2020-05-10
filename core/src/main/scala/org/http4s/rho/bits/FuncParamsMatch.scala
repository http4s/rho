package org.http4s
package rho.bits

import shapeless.HList

import scala.annotation.implicitNotFound

@implicitNotFound("Expecting a function with parameter types matching (${T}) or (Request[F] :: ${T}). Got (${FU}).")
trait FuncParamsMatch[F[_], T <: HList, -FU]

trait FuncParamsMatchers[F[_]] {

  import shapeless._
  import shapeless.ops.function._
  import shapeless.ops.hlist._

  implicit def constantParamsMatch[R]: FuncParamsMatch[F, HNil, R] = new FuncParamsMatch[F, HNil, R] {}

  implicit def functionParamsMatch[T <: HList, TR <: HList, FU, R](implicit fp: FnToProduct.Aux[FU, TR => R], rev: Reverse.Aux[T, TR]): FuncParamsMatch[F, T, FU] = new FuncParamsMatch[F, T, FU] {}

  implicit def functionWithRequestParamsMatch[T <: HList, TR <: HList, FU, R](implicit fp: FnToProduct.Aux[FU, Request[F] :: TR => R], rev: Reverse.Aux[T, TR]): FuncParamsMatch[F, T, FU] = new FuncParamsMatch[F, T, FU] {}
}
