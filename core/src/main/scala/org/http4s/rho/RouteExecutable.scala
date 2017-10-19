package org.http4s
package rho

import org.http4s.rho.bits.HListToFunc
import shapeless.HList

/** Object which can be compiled into a complete route
  * The [[RouteExecutable]] is a complete set of HTTP method, path, query, and headers
  * needed for a compiler to generate a complete route description.
  * @tparam T The `HList` representation of the types the route expects to extract
  *           from a `Request`.
  */
trait RouteExecutable[F[_], T <: HList] extends TypedBuilder[F, T] { exec =>

  /** `Method` of the incoming HTTP `Request` */
  def method: Method

//  /** Create a [[RhoRoute]] from this [[RouteExecutable]] with the provided converters */
  def makeRoute(action: Action[F, T]): RhoRoute[F, T]

  /** Compiles a HTTP request definition into an action */
  final def |>>[FU, R](f: FU)(implicit hltf: HListToFunc[F, T, FU], srvc: CompileService[F, R]): R =
    srvc.compile(makeRoute(hltf.toAction(f)))
}
