package org.http4s
package rho.bits

import scala.language.higherKinds

import shapeless.HList
import shapeless.ops.hlist.Prepend

/** Base trait which is capable of appending header rules
  * @tparam T The `HList` representation of the values to be extracted from the `Request`.
  */
trait HeaderAppendable[T <: HList] {
  type HeaderAppendResult[T <: HList] <: HeaderAppendable[T]

  /** Append the header to the builder, generating a new typed representation of the route */
  def >>>[T1 <: HList](header: TypedHeader[T1])(implicit prep: Prepend[T1, T]): HeaderAppendResult[prep.Out]

  /** Append the header to the builder, generating a new typed representation of the route */
  final def validate[T1 <: HList](header: TypedHeader[T1])(implicit prep: Prepend[T1, T]): HeaderAppendResult[prep.Out] =
    >>>(header)(prep)
}
