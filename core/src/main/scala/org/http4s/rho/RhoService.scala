package org.http4s
package rho

import org.http4s.rho.CompileService.ServiceBuilder
import org.http4s.server.HttpService

import org.log4s.getLogger
import shapeless.HList

class RhoService(routes: Seq[RhoRoute[_ <: HList]] = Vector.empty)
    extends bits.MethodAliases
    with bits.ResponseGeneratorInstances
{
  final protected val logger = getLogger

  implicit final protected val compileService: ServiceBuilder = CompileService.ServiceBuilder()

  // Append the incoming routes
  compileService.append(routes)

  final def and(other: RhoService): RhoService = and(other.getRoutes())

  final def and(routes: Seq[RhoRoute[_ <: HList]]): RhoService = new RhoService(getRoutes() ++ routes)

  final def getRoutes(): Seq[RhoRoute[_ <: HList]] = compileService.routes()

  final def toService(filter: RhoMiddleware = identity): HttpService = compileService.toService(filter)

  final override def toString(): String = s"RhoService(${compileService.routes().toString()})"
}

