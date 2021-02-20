package com.http4s.rho.hal.plus.swagger.demo

import cats.effect.{Blocker, ContextShift, IO, Timer}
import org.http4s.HttpRoutes
import org.http4s.rho.RhoMiddleware
import org.http4s.rho.swagger.syntax.io._

class Routes(businessLayer: BusinessLayer, blocker: Blocker)(implicit
    T: Timer[IO],
    cs: ContextShift[IO]) {

  val middleware: RhoMiddleware[IO] =
    createRhoMiddleware()

  val dynamicContent: HttpRoutes[IO] =
    new RestRoutes[IO](businessLayer).toRoutes(middleware)

  /** Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
    * but its nice to keep it self contained
    */
  val staticContent: HttpRoutes[IO] =
    new StaticContentService[IO](org.http4s.dsl.io, blocker).routes

}
