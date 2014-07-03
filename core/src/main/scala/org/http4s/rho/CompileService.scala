package org.http4s
package rho

import shapeless.HList

trait CompileService[F, A] {
  def compile(action: RhoAction[_ <: HList, F]): A
}
