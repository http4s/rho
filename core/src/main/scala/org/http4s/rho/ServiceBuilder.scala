package org.http4s.rho

import org.http4s._
import org.log4s._
import shapeless.HList

import scala.collection.immutable.VectorBuilder

/** CompileService which accumulates routes and can build a `HttpService` */
class ServiceBuilder (internalRoutes: VectorBuilder[RhoRoute.Tpe]) extends CompileService[RhoRoute.Tpe] {
  private val logger = getLogger

  /** Turn the accumulated routes into an `HttpService`
    *
    * @param filter [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `HttpService` which can be mounted by http4s servers.
    */
  def toService(filter: RhoMiddleware = identity): HttpService =
    CompileService.foldServices(internalRoutes.result(), filter)

  /** Get a snapshot of the currently acquired routes */
  def routes(): Seq[RhoRoute.Tpe] = internalRoutes.result()

  /** Append the routes into this [[ServiceBuilder]]
    *
    * @param routes Routes to accumulate.
    * @return `this` instance with its internal state mutated.
    */
  def append(routes: TraversableOnce[RhoRoute.Tpe]): this.type = {
    internalRoutes ++= routes
    this
  }

  /** Accumulate the [[RhoRoute]] into this [[ServiceBuilder]]
    *
    * This is the same as appending a the single route and returning the same route.
    *
    * @param route [[RhoRoute]] to compile.
    * @tparam T `HList` representation of the result of the route
    * @return The [[RhoRoute]] passed to the method.
    */
  override def compile[T <: HList](route: RhoRoute[T]): RhoRoute[T] = {
    internalRoutes += route
    route
  }
}

object ServiceBuilder {
  /** Constructor method for new `ServiceBuilder` instances */
  def apply(): ServiceBuilder = apply(Seq.empty)

  /** Constructor method for new `ServiceBuilder` instances with existing routes */
  def apply(routes: Seq[RhoRoute.Tpe]): ServiceBuilder = {
    val builder = new VectorBuilder[RhoRoute.Tpe]
    builder ++= routes
    new ServiceBuilder(builder)
  }
}