package org.http4s
package rho

import org.http4s.rho.CompileService.ServiceBuilder
import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.server.HttpService

import org.log4s.getLogger
import shapeless.{HNil, HList}

/** Constructor class for defining routes
  *
  * The [[RhoService]] provides a convenient way to define routes in a style
  * similar to scalatra etc by providing implicit conversions and an implicit
  * [[CompileService]] inside the constructor.
  *
  * {{{
  *   val srvc = new RhoService {
  *     POST / "foo" / pathVar[Int] +? param[String]("param") |>> { (p1: Int, param: String) =>
  *       Ok("success")
  *     }
  *   }
  *
  * }}}
  *
  * @param routes Routes to prepend before elements in the constructor.
  */
class RhoService(routes: Seq[RhoRoute[_ <: HList]] = Vector.empty)
    extends bits.MethodAliases
    with bits.ResponseGeneratorInstances
    with RoutePrependable[RhoService]
{
  final protected val logger = getLogger

  implicit final protected val compileService: ServiceBuilder = CompileService.ServiceBuilder()

  // Append the incoming routes
  compileService.append(routes)

  /** Create a new [[RhoService]] by appending the routes of the passed [[RhoService]]
    *
    * @param other [[RhoService]] whos routes are to be appended.
    * @return A new [[RhoService]] that contains the routes of the other service appended
    *         the the routes contained in this service.
    */
  final def and(other: RhoService): RhoService = and(other.getRoutes())

  /** Create a new [[RhoService]] by append the routes of the passed [[RhoService]]
    *
    * @param routes `Seq` of [[RhoRoute]]'s to be appended.
    * @return A new [[RhoService]] that contains the provided routes appended
    *         the the routes contained in this service.
    */
  final def and(routes: Seq[RhoRoute[_ <: HList]]): RhoService = new RhoService(getRoutes() ++ routes)

  /** Get the collection of [[RhoRoute]]'s accumulated so far */
  final def getRoutes(): Seq[RhoRoute[_ <: HList]] = compileService.routes()

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpService` */
  final def toService(filter: RhoMiddleware = identity): HttpService = compileService.toService(filter)

  final override def toString(): String = s"RhoService(${compileService.routes().toString()})"

  final override def /:(prefix: TypedPath[HNil]): RhoService = {
    new RhoService(compileService.routes().map { prefix /: _ })
  }
}

