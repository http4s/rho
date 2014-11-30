package org.http4s.rho

import bits.HeaderAST._

import shapeless.HList
import shapeless.ops.hlist.Prepend

trait Validatable[T <: HList] {

  /** Checks that specific requirements of a HTTP request header are met */
  def validate[T1 <: HList](header: TypedHeader[T1])(implicit prep: Prepend[T1, T]): Router[prep.Out]

  /** Alias for validate */
  final def >>>[T1 <: HList](header: TypedHeader[T1])(implicit prep: Prepend[T1, T]): Router[prep.Out] =
    validate(header)
}
