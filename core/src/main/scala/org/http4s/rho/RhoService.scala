package org.http4s
package rho

import org.http4s.rho.bits.PathAST.TypedPath

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
    with EntityEncoderInstances
{
  final private val serviceBuilder = ServiceBuilder(routes)

  final protected val logger = getLogger

  final implicit protected def compileService: CompileService[RhoRoute.Tpe] = serviceBuilder

  /** Create a new [[RhoService]] by appending the routes of the passed [[RhoService]]
    *
    * @param other [[RhoService]] whos routes are to be appended.
    * @return A new [[RhoService]] that contains the routes of the other service appended
    *         the the routes contained in this service.
    */
  final def and(other: RhoService): RhoService = new RhoService(this.getRoutes ++ other.getRoutes)

  /** Get a snapshot of the collection of [[RhoRoute]]'s accumulated so far */
  final def getRoutes: Seq[RhoRoute[_ <: HList]] = serviceBuilder.routes()

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpService` */
  final def toService(filter: RhoMiddleware = identity): HttpService = serviceBuilder.toService(filter)

  final override def toString: String = s"RhoService(${serviceBuilder.routes().toString()})"

  final override def /:(prefix: TypedPath[HNil]): RhoService = {
    new RhoService(serviceBuilder.routes().map { prefix /: _ })
  }
}

