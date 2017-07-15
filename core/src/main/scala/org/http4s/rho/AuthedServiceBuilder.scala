package org.http4s.rho

import org.http4s._
import org.http4s.rho.CompileService.foldServices
import org.http4s.rho.bits.PathTree
import org.log4s._
import shapeless.HList

import scala.collection.immutable.VectorBuilder

/** CompileService which accumulates routes and can build a `AuthedService` */
final class AuthedServiceBuilder[U] private(internalRoutes: VectorBuilder[RhoRoute.Tpe]) extends ServiceBuilder(internalRoutes) {

  final val authKey = AttributeKey[U]("authInfo")

  /** Turn the accumulated routes into an `AuthedService`
    *
    * @param filter [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `AuthedService` which can be mounted by http4s servers.
    */
  def toAuthedService(filter: RhoMiddleware = identity): AuthedService[U] = {
      val rhoService = CompileService.foldServices(internalRoutes.result(), filter)
      Service.lift { case AuthedRequest(authInfo, req) =>
        rhoService(req.withAttribute[U](authKey, authInfo))
      }
  }
}

object AuthedServiceBuilder {
  /** Constructor method for new `AuthedServiceBuilder` instances */
  def apply[U](): AuthedServiceBuilder[U] = apply(Seq.empty)

  /** Constructor method for new `AuthedServiceBuilder` instances with existing routes */
  def apply[U](routes: Seq[RhoRoute.Tpe]): AuthedServiceBuilder[U] = {
    val builder = new VectorBuilder[RhoRoute.Tpe]
    builder ++= routes
    new AuthedServiceBuilder(builder)
  }
}