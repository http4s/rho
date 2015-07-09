package org.http4s
package rho

import shapeless.HList

/** This trait serves to transform a [[RhoRoute]] into an `A`
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileService[A] {
  def compile(route: RhoRoute[_ <: HList]): A
}
