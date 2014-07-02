package org.http4s
package rho

import shapeless.HList

trait CompileService[A] {
  def compile[T <: HList, F, O](action: RhoAction[T, F, O]): A
}