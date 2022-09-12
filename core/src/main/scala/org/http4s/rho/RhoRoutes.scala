package org.http4s
package rho

import scala.collection.immutable.Seq
import cats._
import org.http4s.rho.bits.PathAST.TypedPath
import shapeless.{HList, HNil}

/** Constructor class for defining routes
  *
  * The [[RhoRoutes]] provides a convenient way to define routes in a style
  * similar to scalatra etc by providing implicit conversions and an implicit
  * [[CompileRoutes]] inside the constructor.
  *
  * {{{
  *   new RhoRoutes[IO] {
  *     POST / "foo" / pathVar[Int] +? param[String]("param") |>> { (p1: Int, param: String) =>
  *       Ok("success")
  *     }
  *   }
  * }}}
  *
  * @param routes Routes to prepend before elements in the constructor.
  */
class RhoRoutes[F[_]: Monad](routes: Seq[RhoRoute[F, _ <: HList]] = Vector.empty)
    extends bits.MethodAliases
    with bits.ResponseGeneratorInstances[F]
    with RoutePrependable[F, RhoRoutes[F]]
    with RhoDsl[F] {
  final private val routesBuilder = RoutesBuilder[F](routes)
  
  final implicit protected def compileRoutes: CompileRoutes[F, RhoRoute.Tpe[F]] = routesBuilder

  /** Create a new [[RhoRoutes]] by appending the routes of the passed [[RhoRoutes]]
    *
    * @param other [[RhoRoutes]] whos routes are to be appended.
    * @return A new [[RhoRoutes]] that contains the routes of the other service appended
    *         the the routes contained in this service.
    */
  final def and(other: RhoRoutes[F]): RhoRoutes[F] = new RhoRoutes(
    this.getRoutes ++ other.getRoutes
  )

  /** Get a snapshot of the collection of [[RhoRoute]]'s accumulated so far */
  final def getRoutes: Seq[RhoRoute[F, _ <: HList]] = routesBuilder.routes()

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpRoutes` */
  final def toRoutes(middleware: RhoMiddleware[F] = identity): HttpRoutes[F] =
    routesBuilder.toRoutes(middleware)

  final override def toString: String = s"RhoRoutes(${routesBuilder.routes().toString()})"

  final override def /:(prefix: TypedPath[F, HNil]): RhoRoutes[F] =
    new RhoRoutes(routesBuilder.routes().map(prefix /: _))
}
