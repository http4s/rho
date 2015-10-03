package org.http4s
package rho

import org.log4s.getLogger

import org.http4s.rho.bits.PathTree
import org.http4s.server.{Service, HttpService}
import shapeless.HList

/** This trait serves to transform a [[RhoRoute]] into an `A`
  * This can be a stateful operation, storing the action for later execution
  * or any other type of compilation phase.
  */
trait CompileService[A] {
  def compile[T <: HList](route: RhoRoute[T]): A
}

object CompileService {

  val identityCompiler = new CompileService[RhoRoute.Tpe] {
    def compile[T <: HList](route: RhoRoute[T]) = route
  }

  object Implicit {
    implicit val compiler: CompileService[RhoRoute.Tpe] = identityCompiler
  }


  // this feels like it should go somewhere else
  def makeService(routes: Seq[RhoRoute.Tpe], filter: RouteMiddleWare = identity): HttpService = {
    val tree = filter(routes).foldLeft(PathTree()){ (t, r) => t.appendRoute(r) }
      Service.lift { req => tree.getResult(req).toResponse }
  }

  final case class ServiceBuilder() extends CompileService[RhoRoute.Tpe] {
    private val logger = getLogger
    private val internalRoutes = Vector.newBuilder[RhoRoute.Tpe]

    final def toService(filter: RouteMiddleWare = identity): HttpService =
      makeService(internalRoutes.result(), filter)

    def routes(): Seq[RhoRoute.Tpe] = internalRoutes.synchronized(internalRoutes.result())

    def append(routes: TraversableOnce[RhoRoute.Tpe]): this.type = {
      internalRoutes ++= routes
      this
    }

    def compile[T <: HList](route: RhoRoute[T]) = internalRoutes.synchronized {
      internalRoutes += route
      route
    }
  }
}
