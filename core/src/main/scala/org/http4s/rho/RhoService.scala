package org.http4s
package rho

import org.http4s.server.HttpService

import org.log4s.getLogger

class RhoService(routes: Seq[RhoRoute] = Vector.empty)
    extends bits.MethodAliases
    with bits.ResponseGeneratorInstances
{
  private val _srvc = CompileService.ServiceBuilder()

  final protected val logger = getLogger

  implicit protected def compilerSrvc: CompileService[RhoRoute] = _srvc

  final def and(other: RhoService): RhoService = and(other.getRoutes())

  final def and(routes: Seq[RhoRoute]): RhoService = new RhoService(getRoutes() ++ routes)

  final def getRoutes(): Seq[RhoRoute] = _srvc.routes()

  final def toService(filter: RouteMiddleWare = identity): HttpService = _srvc.toService(filter)

  final override def toString(): String = s"RhoService(${_srvc.routes().toString()})"
}

