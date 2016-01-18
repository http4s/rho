package org.http4s
package rho

import org.log4s.getLogger

import org.http4s.rho.bits.PathTree
import org.http4s.{Service, HttpService}
import shapeless.HList

/** This trait serves to transform a [[RhoRoute]] into an `RouteType`
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

  /** Tool for accumulating compiled routes */
  final case class ServiceBuilder() extends CompileService[RhoRoute.Tpe] {
    private val logger = getLogger
    private val internalRoutes = Vector.newBuilder[RhoRoute.Tpe]

    /** Turn the accumulated routes into an `HttpService`
      *
      * @param filter [[RhoMiddleware]] to apply to the collection of routes.
      * @return An `HttpService` which can be mounted by http4s servers.
      */
    final def toService(filter: RhoMiddleware = identity): HttpService =
      foldServices(internalRoutes.result(), filter)

    /** Get the currently acquired routes */
    def routes(): Seq[RhoRoute.Tpe] = internalRoutes.synchronized(internalRoutes.result())

    /** Accumulate the routes into this [[ServiceBuilder]]
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
    override def compile[T <: HList](route: RhoRoute[T]): RhoRoute[T] = internalRoutes.synchronized {
      internalRoutes += route
      route
    }
  }
}
