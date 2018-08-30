package org.http4s
package rho

import cats.Monad
import cats.data.Kleisli
import org.http4s.rho.RhoRoute.Tpe
import org.http4s.rho.bits.PathTree
import shapeless.HList

/** Transforms a [[RhoRoute]] into an `RouteType`.
  *
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileService[F[_], RouteType] {

  /** Transform the [[RhoRoute]] into a `RouteType` possibly mutating this compilers state.
    *
    * @param route [[RhoRoute]] to compile.
    * @tparam T `HList` representation of the result of the route
    * @return The result of the compilation process.
    */
  def compile[T <: HList](route: RhoRoute[F, T]): RouteType
}

object CompileService {

  /** [[CompileService]] that simply returns its argument */
  def identityCompiler[F[_]]: CompileService[F, Tpe[F]] = new CompileService[F, RhoRoute.Tpe[F]] {
    def compile[T <: HList](route: RhoRoute[F, T]): RhoRoute[F, T] = route
  }

  /** Importable implicit identity compiler */
  object Implicit {
    implicit def compiler[F[_]]: CompileService[F, RhoRoute.Tpe[F]] = identityCompiler[F]
  }


  /** Convert the `Seq` of [[RhoRoute]]'s into a `HttpService`
    *
    * @param routes `Seq` of routes to bundle into a service.
    * @param filter [[RhoMiddleware]] to apply to the routes.
    * @return An `HttpService`
    */
  def foldServices[F[_]: Monad](routes: Seq[RhoRoute.Tpe[F]], filter: RhoMiddleware[F]): HttpRoutes[F] = {
    val tree = filter(routes).foldLeft(PathTree[F]()){ (t, r) => t.appendRoute(r) }
    Kleisli((req: Request[F]) => tree.getResult(req).toResponse)
  }
}
