package org.http4s
package rho

import org.http4s.rho.bits.{FuncParamsMatch, FuncResultMatch, HListToFunc}
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

  /** Create a [[RhoRoute]] from a given [[Action]] */
  def makeRoute(action: Action[F, T]): RhoRoute[F, T]

  /** Compiles a HTTP request definition into an action */
  final def |>>[U, R](f: U)(implicit dpm: FuncParamsMatch[F, T, U], frm: FuncResultMatch[F, T, U], hltf: HListToFunc[F, T, U], srvc: CompileRoutes[F, R]): R =
    srvc.compile(makeRoute(hltf.toAction(f)))
}
