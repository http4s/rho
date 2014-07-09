package org.http4s
package rho.bits

import org.http4s.rho.bits.HeaderAST.TypedHeader
import shapeless.HList
import shapeless.ops.hlist.Prepend

/** Base trait which is capable of appending header rules
  * @tparam T1 HList representation of the values to be extracted from the [[Request]]
  */
trait HeaderAppendable[T1 <: HList] {
  type Self <: HeaderAppendable[T1]

  /** Append the header to the builder, generating a new typed representation of the route */
  def >>>[T2 <: HList](header: TypedHeader[T2])(implicit prep: Prepend[T2, T1]): HeaderAppendable[prep.Out]
}