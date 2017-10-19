package org.http4s
package rho

import cats.Applicative
import cats.data.{Kleisli, OptionT}
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
  def identityCompiler[F[_]] = new CompileService[F, RhoRoute.Tpe[F]] {
    def compile[T <: HList](route: RhoRoute[F, T]) = route
  }

  /** Importable implicit identity compiler */
  object Implicit {
    // TODO: check if this is a problem, does this implicit work?

    implicit def compiler[F[_]]: CompileService[F, RhoRoute.Tpe[F]] = identityCompiler
  }


  /** Convert the `Seq` of [[RhoRoute]]'s into a `HttpService`
    *
    * @param routes `Seq` of routes to bundle into a service.
    * @param filter [[RhoMiddleware]] to apply to the routes.
    * @return An `HttpService`
    */
  def foldServices[F[_]](routes: Seq[RhoRoute.Tpe[F]], filter: RhoMiddleware[F]): HttpService[F] = {
    ??? // TODO: fix impl

//    val tree = filter(routes).foldLeft(PathTree()){ (t, r) => t.appendRoute(r) }
//    Service.lift { req => tree.getResult(req).toResponse }
  }
}
