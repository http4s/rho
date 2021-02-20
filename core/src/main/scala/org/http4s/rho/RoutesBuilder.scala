package org.http4s.rho

import scala.collection.immutable.VectorBuilder
import scala.collection.immutable.Seq
import cats._
import shapeless.HList
import org.http4s._

import scala.collection.compat._

/** CompileRoutes which accumulates routes and can build a `HttpRoutes` */
final class RoutesBuilder[F[_]: Defer: Monad] private (
    internalRoutes: VectorBuilder[RhoRoute.Tpe[F]])
    extends CompileRoutes[F, RhoRoute.Tpe[F]] {

  /** Turn the accumulated routes into an `HttpRoutes`
    *
    * @param middleware [[RhoMiddleware]] to apply to the collection of routes.
    * @return An `HttpRoutes` which can be mounted by http4s servers.
    */
  def toRoutes(middleware: RhoMiddleware[F] = identity): HttpRoutes[F] =
    CompileRoutes.foldRoutes(middleware.apply(internalRoutes.result()))

  /** Get a snapshot of the currently acquired routes */
  def routes(): Seq[RhoRoute.Tpe[F]] = internalRoutes.result()

  /** Append the routes into this [[RoutesBuilder]]
    *
    * @param routes Routes to accumulate.
    * @return `this` instance with its internal state mutated.
    */
  def append(routes: IterableOnce[RhoRoute.Tpe[F]]): this.type = {
    internalRoutes ++= routes.iterator.to(Iterable)
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
  override def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute[F, T] = {
    internalRoutes += route
    route
  }
}

object RoutesBuilder {

  /** Constructor method for new `RoutesBuilder` instances */
  def apply[F[_]: Defer: Monad](): RoutesBuilder[F] = apply(Seq.empty)

  /** Constructor method for new `RoutesBuilder` instances with existing routes */
  def apply[F[_]: Defer: Monad](routes: Seq[RhoRoute.Tpe[F]]): RoutesBuilder[F] = {
    val builder = new VectorBuilder[RhoRoute.Tpe[F]]
    builder ++= routes

    new RoutesBuilder(builder)
  }
}
