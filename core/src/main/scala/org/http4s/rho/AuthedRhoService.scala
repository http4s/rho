package org.http4s
package rho

import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.AuthedService
import org.log4s.getLogger
import shapeless.{HNil, HList}

/** Constructor class for defining routes
  *
  * The [[AuthedRhoService]] provides a convenient way to define routes in a style
  * similar to scalatra etc by providing implicit conversions and an implicit
  * [[CompileService]] inside the constructor.
  *
  * {{{
  *   val srvc = new AuthedRhoService[U] {
  *     POST / "foo" / pathVar[Int] +? param[String]("param") |>> { (req: Request, p1: Int, param: String) =>
  *       withAuth(req) { authInfo: U =>
  *         Ok("success")
  *       }
  *     }
  *   }
  *
  * }}}
  *
  * @tparam U [[AuthInfo]] type for this service.
  * @param routes Routes to prepend before elements in the constructor.
  * @param ev implicit AuthInfo typeclass instance for U
  *
  */
class AuthedRhoService[U](routes: Seq[RhoRoute[_ <: HList]] = Vector.empty)(implicit ev: AuthInfo[U])
  extends bits.MethodAliases
    with bits.ResponseGeneratorInstances
    with RoutePrependable[AuthedRhoService[U]]
    with EntityEncoderInstances
{
  final private val serviceBuilder = AuthedServiceBuilder[U](routes, ev)

  final protected val logger = getLogger

  final implicit protected def compileService: CompileService[RhoRoute.Tpe] = serviceBuilder

  /** Create a new [[AuthedRhoService]] by appending the routes of the passed [[AuthedRhoService]]
    *
    * @param other [[AuthedRhoService]] whos routes are to be appended.
    * @return A new [[AuthedRhoService]] that contains the routes of the other service appended
    *         the the routes contained in this service.
    */
  final def and(other: AuthedRhoService[U]): AuthedRhoService[U] = new AuthedRhoService(this.getRoutes ++ other.getRoutes)(ev)

  /** Get a snapshot of the collection of [[RhoRoute]]'s accumulated so far */
  final def getRoutes: Seq[RhoRoute[_ <: HList]] = serviceBuilder.routes()

  /** Convert the [[RhoRoute]]'s accumulated into a `AuthedService` */
  final def toService(filter: RhoMiddleware = identity): AuthedService[U] = serviceBuilder.toAuthedService(filter)

  final override def toString: String = s"AuthedRhoService(${serviceBuilder.routes().toString()})"

  final override def /:(prefix: TypedPath[HNil]): AuthedRhoService[U] = {
    new AuthedRhoService(serviceBuilder.routes().map { prefix /: _ })(ev)
  }

  def withAuth[R](req: Request)(f: U => R): R = {
    val user = ev.fromString(req.attributes.get(AttributeKey[String](ev.attrKey)).get)
    f(user)
  }
}

