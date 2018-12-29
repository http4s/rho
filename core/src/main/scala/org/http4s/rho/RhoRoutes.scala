package org.http4s
package rho

import cats.Monad
import org.http4s.rho.bits.{AuthedMatchersHListToFunc, MatchersHListToFunc}
import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.server.AuthMiddleware
import shapeless.{HList, HNil}
import scala.collection.immutable.Seq

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
class RhoRoutes[F[_]: Monad](routes: Seq[UnAuthedRhoRoute[F, _ <: HList]] = Vector.empty)
    extends bits.MethodAliases
    with bits.ResponseGeneratorInstances[F]
    with RoutePrependable[F, RhoRoutes[F]]
    with RhoDsl[F]
    with MatchersHListToFunc[F]
{
  final private val routesBuilder: RoutesBuilder[F, UnAuthedRhoRoute] = RoutesBuilder[F, UnAuthedRhoRoute](routes)

  final implicit protected def compileRoutes: CompileRoutes.Aux[F, UnAuthedRhoRoute, UnAuthedRhoRoute.Tpe[F]] = routesBuilder

  /** Create a new [[RhoRoutes]] by appending the routes of the passed [[RhoRoutes]]
    *
    * @param other [[RhoRoutes]] whos routes are to be appended.
    * @return A new [[RhoRoutes]] that contains the routes of the other service appended
    *         the the routes contained in this service.
    */
  final def and(other: RhoRoutes[F]): RhoRoutes[F] = new RhoRoutes(this.getRoutes ++ other.getRoutes)

  final def and(other: CompositeRhoRoutes[F]): CompositeRhoRoutes[F] =
    new CompositeRhoRoutes(this.getRoutes ++ other.routes)


  /** Get a snapshot of the collection of [[RhoRoute]]'s accumulated so far */
  final def getRoutes: Seq[UnAuthedRhoRoute[F, _ <: HList]] = routesBuilder.routes()

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpRoutes` */
  final def toHttpRoutes(middleware: ARhoMiddleware[F, UnAuthedRhoRoute] = identity): HttpRoutes[F] =
    routesBuilder.toHttpRoutes(middleware)

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpRoutes` */
  final def toHttpRoutes(middleware: RhoMiddleware[F])(implicit dummyImplicit: DummyImplicit): HttpRoutes[F] =
    new CompositeRhoRoutes(getRoutes).toHttpRoutes(middleware)

  final override def toString: String = s"RhoRoutes(${routesBuilder.routes().toString()})"

  final override def /:(prefix: TypedPath[F, HNil]): RhoRoutes[F] = {
    new RhoRoutes(routesBuilder.routes().map { prefix /: _ })
  }
}

class AuthedRhoRoutes[F[_]: Monad, U](routes: Seq[PreAuthedRhoRoute[F, U, _ <: HList]] = Vector.empty)
  extends bits.MethodAliases
    with bits.ResponseGeneratorInstances[F]
    with RoutePrependable[F, AuthedRhoRoutes[F, U]]
    with RhoDsl[F]
    with AuthedMatchersHListToFunc[F, U]
{
  type Tpe[G[_], T <: HList] = PreAuthedRhoRoute[G, U, T]

  final private val routesBuilder: RoutesBuilder[F, Tpe] = RoutesBuilder[F, Tpe](routes)

  final implicit protected def compileRoutes: CompileRoutes.Aux[F, Tpe, PreAuthedRhoRoute.Tpe[F, U]] =
    routesBuilder

  /** Create a new [[RhoRoutes]] by appending the routes of the passed [[RhoRoutes]]
    *
    * @param other [[RhoRoutes]] whos routes are to be appended.
    * @return A new [[RhoRoutes]] that contains the routes of the other service appended
    *         the the routes contained in this service.
    */
  final def and(other: AuthedRhoRoutes[F, U]): AuthedRhoRoutes[F, U] = new AuthedRhoRoutes(this.getRoutes ++ other.getRoutes)

  /** Get a snapshot of the collection of [[RhoRoute]]'s accumulated so far */
  final def getRoutes: Seq[PreAuthedRhoRoute[F, U, _ <: HList]] = routesBuilder.routes()

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpRoutes` */
  final def toHttpRoutes(middleware: ARhoMiddleware[F, Tpe] = identity): AuthedService[U, F] =
    routesBuilder.toHttpRoutes(middleware)(CompileHttpRoutes.authedAction[F, U])

  final def toRhoRoutes(authMiddleware: AuthMiddleware[F, U]): CompositeRhoRoutes[F] = {
    new CompositeRhoRoutes[F](getRoutes.map(_.attachAuthMiddleware(authMiddleware)))
  }

  final override def toString: String = s"AuthedRhoRoutes(${getRoutes.toString()})"

  /** Prepend the prefix to the path rules
    *
    * @param prefix non-capturing prefix to prepend
    * @return builder with the prefix prepended to the path rules
    */
  final override def /:(prefix: TypedPath[F, HNil]): AuthedRhoRoutes[F, U] = {
    new AuthedRhoRoutes(getRoutes.map { prefix /: _ })
  }
}

final class CompositeRhoRoutes[F[_]: Monad](val routes: Seq[RhoRoute[F, _ <:HList]])
  extends RoutePrependable[F, CompositeRhoRoutes[F]] {

  private val routesBuilder: RoutesBuilder[F, RhoRoute] = RoutesBuilder[F, RhoRoute](routes)

  /** Prepend the prefix to the path rules
    *
    * @param prefix non-capturing prefix to prepend
    * @return builder with the prefix prepended to the path rules
    */
  override def /:(prefix: TypedPath[F, HNil]): CompositeRhoRoutes[F] = {
    new CompositeRhoRoutes(routes.map { prefix /: _ })
  }

  def and(other: CompositeRhoRoutes[F]): CompositeRhoRoutes[F] =
    new CompositeRhoRoutes(this.routes ++ other.routes)

  def and(other: RhoRoutes[F]): CompositeRhoRoutes[F] =
    new CompositeRhoRoutes(this.routes ++ other.getRoutes)

  /** Convert the [[RhoRoute]]'s accumulated into a `HttpRoutes` */
  def toHttpRoutes(middleware: RhoMiddleware[F] = identity): HttpRoutes[F] =
    routesBuilder.toHttpRoutes(middleware)(CompileHttpRoutes.action[F])
}