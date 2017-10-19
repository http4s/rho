package org.http4s.rho

import org.http4s._
import org.log4s._
import shapeless.HList

import scala.collection.immutable.VectorBuilder

/** CompileService which accumulates routes and can build a `HttpService` */
final class ServiceBuilder[F[_]] private(internalRoutes: VectorBuilder[RhoRoute.Tpe[F]]) extends CompileService[F, RhoRoute.Tpe[F]] {
  private val logger = getLogger

  /** Turn the accumulated routes into an `HttpService`
    *
    * @param filter [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `HttpService` which can be mounted by http4s servers.
    */
  def toService(filter: RhoMiddleware[F]): HttpService[F] =
    CompileService.foldServices(internalRoutes.result(), filter)

  /** Get a snapshot of the currently acquired routes */
  def routes(): Seq[RhoRoute.Tpe[F]] = internalRoutes.result()

  /** Append the routes into this [[ServiceBuilder]]
    *
    * @param routes Routes to accumulate.
    * @return `this` instance with its internal state mutated.
    */
  def append(routes: TraversableOnce[RhoRoute.Tpe[F]]): this.type = {
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
  override def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute[F, T] = {
    internalRoutes += route
    route
  }
}

object ServiceBuilder {
  /** Constructor method for new `ServiceBuilder` instances */
  def apply[F[_]](): ServiceBuilder[F] = apply(Seq.empty)

  /** Constructor method for new `ServiceBuilder` instances with existing routes */
  def apply[F[_]](routes: Seq[RhoRoute.Tpe[F]]): ServiceBuilder[F] = {
    val builder = new VectorBuilder[RhoRoute.Tpe[F]]
    builder ++= routes
    new ServiceBuilder(builder)
  }
}
