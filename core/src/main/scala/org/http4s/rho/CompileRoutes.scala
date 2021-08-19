package org.http4s
package rho

import scala.collection.immutable.Seq
import cats._
import shapeless.HList
import org.http4s.rho.RhoRoute.Tpe
import org.http4s.rho.bits.PathTree

/** Transforms a [[RhoRoute]] into an `RouteType`.
  *
  * This can be a stateful operation, storing the action for later execution or any other type of
  * compilation phase.
  */
trait CompileRoutes[F[_], RouteType] {

  /** Transform the [[RhoRoute]] into a `RouteType` possibly mutating this compilers state.
    *
    * @param route
    *   [[RhoRoute]] to compile.
    * @tparam T
    *   `HList` representation of the result of the route
    * @return
    *   The result of the compilation process.
    */
  def compile[T <: HList](route: RhoRoute[F, T]): RouteType
}

object CompileRoutes {

  /** [[CompileRoutes]] that simply returns its argument */
  def identityCompiler[F[_]]: CompileRoutes[F, Tpe[F]] = new CompileRoutes[F, RhoRoute.Tpe[F]] {
    def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute[F, T] = route
  }

  /** Importable implicit identity compiler */
  object Implicit {
    implicit def compiler[F[_]]: CompileRoutes[F, RhoRoute.Tpe[F]] = identityCompiler[F]
  }

  /** Convert the `Seq` of [[RhoRoute]] 's into a `HttpRoutes`
    *
    * @param routes
    *   `Seq` of routes to bundle into a service.
    * @return
    *   An `HttpRoutes`
    */
  def foldRoutes[F[_]: Monad](routes: Seq[RhoRoute.Tpe[F]]): HttpRoutes[F] = {
    val tree = routes.foldLeft(PathTree[F]())((t, r) => t.appendRoute(r))
    HttpRoutes((req: Request[F]) => tree.getResult(req).toResponse)
  }
}
