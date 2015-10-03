package org.http4s
package rho

import org.log4s.getLogger

import org.http4s.rho.bits.PathTree
import org.http4s.server.{Service, HttpService}

/** This trait serves to transform a [[RhoRoute]] into an `A`
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileService[A] {
  def compile(route: RhoRoute): A
}

object CompileService {

  val identityCompiler = new CompileService[RhoRoute] {
    def compile(route: RhoRoute) = route
  }

  object Implicit {
    implicit val compiler: CompileService[RhoRoute] = identityCompiler
  }


  // this feels like it should go somewhere else
  def makeService(routes: Seq[RhoRoute], filter: RouteMiddleWare = identity): HttpService = {
    val tree = filter(routes).foldLeft(PathTree()){ (t, r) => t.appendRoute(r) }
      Service.lift { req => tree.getResult(req).toResponse }
  }

  final case class ServiceBuilder() extends CompileService[RhoRoute] {
    private val logger = getLogger
    private val internalRoutes = Vector.newBuilder[RhoRoute]

    final def toService(filter: RouteMiddleWare = identity): HttpService =
      makeService(internalRoutes.result(), filter)

    def routes(): Seq[RhoRoute] = internalRoutes.synchronized(internalRoutes.result())

    def append(routes: TraversableOnce[RhoRoute]): this.type = {
      internalRoutes ++= routes
      this
    }

    def compile(route: RhoRoute) = internalRoutes.synchronized {
      internalRoutes += route
      route
    }
  }
}
