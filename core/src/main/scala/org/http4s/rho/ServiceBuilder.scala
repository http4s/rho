package org.http4s.rho

import scala.collection.immutable.VectorBuilder
import cats.Monad
import shapeless.HList
import org.http4s._

/** CompileService which accumulates routes and can build a `HttpRoutes` */
final class ServiceBuilder[F[_]: Monad] private(internalRoutes: VectorBuilder[RhoRoute.Tpe[F]]) extends CompileRoutes[F, RhoRoute.Tpe[F]] {

  /** Turn the accumulated routes into an `HttpRoutes`
    *
    * @param middleware [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `HttpRoutes` which can be mounted by http4s servers.
    */
  def toRoutes(middleware: RhoMiddleware[F] = identity): HttpRoutes[F] =
    CompileRoutes.foldRoutes(middleware.apply(internalRoutes.result()))

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
  def apply[F[_]: Monad](): ServiceBuilder[F] = apply(Seq.empty)

  /** Constructor method for new `ServiceBuilder` instances with existing routes */
  def apply[F[_]: Monad](routes: Seq[RhoRoute.Tpe[F]]): ServiceBuilder[F] = {
    val builder = new VectorBuilder[RhoRoute.Tpe[F]]
    builder ++= routes

    new ServiceBuilder(builder)
  }
}
