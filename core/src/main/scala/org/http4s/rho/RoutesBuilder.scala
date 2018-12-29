package org.http4s.rho

import scala.collection.immutable.{VectorBuilder, Seq}
import cats.Monad
import shapeless.HList

/** CompileRoutes which accumulates routes and can build a `HttpRoutes` */
final class RoutesBuilder[F[_]: Monad, RouteType[_[_], _ <: HList]] private (internalRoutes: VectorBuilder[RouteType[F, _ <: HList]])
extends CompileRoutes[F, RouteType[F, _ <: HList]] {

  override type InRouteType[G[_], T <: HList] = RouteType[G, T]

  /** Turn the accumulated routes into an `HttpRoutes`
    *
    * @param middleware [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `HttpRoutes` which can be mounted by http4s servers.
    */
  def toHttpRoutes(middleware: ARhoMiddleware[F, RouteType] = identity)(implicit compileHttpRoutes: CompileHttpRoutes[F, RouteType]): compileHttpRoutes.Out =
    compileHttpRoutes.foldRoutes(middleware.apply(internalRoutes.result()))

  /** Get a snapshot of the currently acquired routes */
  def routes(): Seq[RouteType[F, _ <: HList]] = internalRoutes.result()

  /** Append the routes into this [[RoutesBuilder]]
    *
    * @param routes Routes to accumulate.
    * @return `this` instance with its internal state mutated.
    */
  def append(routes: TraversableOnce[RouteType[F, _ <: HList]]): this.type = {
    internalRoutes ++= routes
    this
  }

  /** Accumulate the [[RhoRoute]] into this [[RoutesBuilder]]
    *
    * This is the same as appending a the single route and returning the same route.
    *
    * @param route [[RhoRoute]] to compile.
    * @tparam T `HList` representation of the result of the route
    * @return The [[RhoRoute]] passed to the method.
    */
  override def compile[T <: HList](route: RouteType[F, T]): RouteType[F, T] = {
    internalRoutes += route
    route
  }
}

object RoutesBuilder {
  /** Constructor method for new `RoutesBuilder` instances */
  def apply[F[_]: Monad, R[_[_], _ <: HList]](): RoutesBuilder[F, R] = apply(Seq.empty)

  /** Constructor method for new `RoutesBuilder` instances with existing routes */
  def apply[F[_]: Monad, R[_[_], _ <: HList]](routes: Seq[R[F, _ <: HList]]): RoutesBuilder[F, R] = {
    val builder = new VectorBuilder[R[F, _ <: HList]]
    builder ++= routes

    new RoutesBuilder[F, R](builder)
  }
}
