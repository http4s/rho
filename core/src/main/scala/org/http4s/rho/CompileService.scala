package org.http4s
package rho

import shapeless.HList

/** This trait serves to transform a [[RhoAction]] into an `A`
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileService[A] {
  def compile(action: RhoAction[_ <: HList]): A
}
