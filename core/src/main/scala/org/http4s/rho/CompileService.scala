package org.http4s
package rho

import shapeless.HList

trait CompileService[F, A] {
  def compile[T <: HList](action: RhoAction[T, F]): A
}
