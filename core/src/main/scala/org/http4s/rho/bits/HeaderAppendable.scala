package org.http4s
package rho.bits

import HeaderAST.HeaderRule
import shapeless.HList
import shapeless.ops.hlist.Prepend


private[rho] trait HeaderAppendable[T1 <: HList] {
  type Self <: HeaderAppendable[T1]

  def >>>[T2 <: HList](v: HeaderRule[T2])(implicit prep1: Prepend[T2, T1]): HeaderAppendable[prep1.Out]
}