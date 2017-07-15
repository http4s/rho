package org.http4s
package rho

import org.http4s.rho.bits.PathTree
import shapeless.HList

/** Transforms a [[RhoRoute]] into an `RouteType`.
  *
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileService[RouteType] {

  /** Transform the [[RhoRoute]] into a `RouteType` possibly mutating this compilers state.
    *
    * @param route [[RhoRoute]] to compile.
    * @tparam T `HList` representation of the result of the route
    * @return The result of the compilation process.
    */
  def compile[T <: HList](route: RhoRoute[T]): RouteType
}

object CompileService {

  /** [[CompileService]] that simply returns its argument */
  val identityCompiler = new CompileService[RhoRoute.Tpe] {
    def compile[T <: HList](route: RhoRoute[T]) = route
  }

  /** Importable implicit identity compiler */
  object Implicit {
    implicit val compiler: CompileService[RhoRoute.Tpe] = identityCompiler
  }


  /** Convert the `Seq` of [[RhoRoute]]'s into a `HttpService`
    *
    * @param routes `Seq` of routes to bundle into a service.
    * @param filter [[RhoMiddleware]] to apply to the routes.
    * @return An `HttpService`
    */
  def foldServices(routes: Seq[RhoRoute.Tpe], filter: RhoMiddleware = identity): HttpService = {
    val tree = filter(routes).foldLeft(PathTree()){ (t, r) => t.appendRoute(r) }
    Service.lift { req => tree.getResult(req).toResponse }
  }
}
