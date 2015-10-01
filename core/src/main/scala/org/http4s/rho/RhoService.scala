package org.http4s
package rho

import org.http4s.rho.bits._
import org.http4s.server.{ Service, HttpService }

import org.log4s.{Logger, getLogger}
import shapeless.HList
import scalaz.concurrent.Task

class RhoService(routes: Seq[RhoRoute[_ <: HList]] = Nil) extends bits.MethodAliases
                          with bits.ResponseGeneratorInstances
{
  final protected val logger = getLogger

  private val internalRoutes = Vector.newBuilder[RhoRoute[_ <: HList]]
  internalRoutes ++= routes

  implicit protected def compilerSrvc[T <: HList] = new CompileService[T, Action[T]] {
    override def compile(route: RhoRoute[T]): Action[T] = internalRoutes.synchronized {
      internalRoutes += route
      route.action
    }
  }

  final def and(other: RhoService): RhoService = and(other.getRoutes())

  final def and(routes: Seq[RhoRoute[_ <: HList]]): RhoService = new RhoService(getRoutes() ++ routes)

  final def getRoutes(): Seq[RhoRoute[_ <: HList]] = internalRoutes.synchronized {
    internalRoutes.result()
  }

  final def toService(filter: RouteMiddleWare = identity): HttpService = {
    val tree = filter(internalRoutes.result()).foldLeft(PathTree()){ (t, r) => t.appendRoute(r) }
    Service.lift(RhoService.findRoute(logger, tree))
  }

  final override def toString(): String = internalRoutes.synchronized {
    s"RhoService(${internalRoutes.result().toString()})"
  }
}

object RhoService {
  private def findRoute(logger: Logger, tree: PathTree)(req: Request): Task[Response] = {
    logger.trace(s"Request: ${req.method}:${req.uri}")
    val routeResult: RouteResult[Task[Response]] = tree.getResult(req)
    routeResult match {
      case SuccessResponse(t) => t
      case NoMatch            => HttpService.notFound
      case FailureResponse(r) => r.toResponse
    }
  }
}
