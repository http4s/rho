package org.http4s.rho

import org.http4s._
import org.log4s._
import shapeless.HList

import scala.collection.immutable.VectorBuilder

/** CompileService which accumulates routes and can build a `AuthedService` */
final class AuthedServiceBuilder[U] private(internalRoutes: VectorBuilder[RhoRoute.Tpe], ev: AuthInfo[U]) extends ServiceBuilder(internalRoutes) {

  /** Turn the accumulated routes into an `AuthedService`
    *
    * @param filter [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `AuthedService` which can be mounted by http4s servers.
    */
  def toAuthedService(filter: RhoMiddleware = identity): AuthedService[U] =
    CompileService.foldAuthedServices(internalRoutes.result(), filter)(ev)
}

object AuthedServiceBuilder {
  /** Constructor method for new `AuthedServiceBuilder` instances */
  def apply[U]()(implicit ev: AuthInfo[U]): AuthedServiceBuilder[U] = apply(Seq.empty, ev)

  /** Constructor method for new `AuthedServiceBuilder` instances with existing routes */
  def apply[U](routes: Seq[RhoRoute.Tpe], ev: AuthInfo[U]): AuthedServiceBuilder[U] = {
    val builder = new VectorBuilder[RhoRoute.Tpe]
    builder ++= routes
    new AuthedServiceBuilder(builder, ev)
  }
}