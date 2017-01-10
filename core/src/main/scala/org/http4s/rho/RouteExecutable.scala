package org.http4s
package rho

import bits.HListToFunc
import org.http4s.HttpService
import shapeless.HList

import scalaz.concurrent.Task

/** Object which can be compiled into a complete route
  * The [[RouteExecutable]] is a complete set of HTTP method, path, query, and headers
  * needed for a compiler to generate a complete route description.
  * @tparam T The `HList` representation of the types the route expects to extract
  *           from a `Request`.
  */
trait RouteExecutable[T <: HList] extends TypedBuilder[T] { exec =>

  /** `Method` of the incoming HTTP `Request` */
  def method: Method

//  /** Create a [[RhoRoute]] from this [[RouteExecutable]] with the provided converters */
  def makeRoute(action: Action[T]): RhoRoute[T]

  /** Compiles a HTTP request definition into an action */
  final def |>>[F, R](f: F)(implicit hltf: HListToFunc[T, F], srvc: CompileService[R]): R =
    srvc.compile(makeRoute(hltf.toAction(f)))
}
