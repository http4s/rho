package org.http4s
package rho

import bits.HListToFunc
import shapeless.HList

import scalaz.concurrent.Task

/** Object which can be compiled into a complete route
 * The [[RouteExecutable]] is a complete set of HTTP method, path, query, and headers
  * needed for a compiler to generate a complete route description.
 * @tparam T the HList representation of the types the route expects to extract
  *           from a [[Request]]
 */
trait RouteExecutable[T <: HList] extends TypedBuilder[T] {

  /** [[Method]] of the incoming HTTP [[Request]] */
  def method: Method

//  /** Create a [[RhoAction]] from this [[RouteExecutable]] with the provided converters */
  def makeAction(action: Action[T]): RhoRoute[T]

  /** Compiles a HTTP request definition into an action */
  final def |>>[R](action: Action[T])(implicit srvc: CompileService[R]): R =
    srvc.compile(makeAction(action))

  /** Provide an action from which to generate a complete route
    * @return a function `Request => Option[Task[Response]]` which can be used as a complete route
    */
  final def runWith(action: Action[T]): Request => Task[Option[Response]] = {
    val srvc = new RhoService { compilerSrvc.compile(makeAction(action)) }.toService
    srvc.apply(_: Request)
  }
}