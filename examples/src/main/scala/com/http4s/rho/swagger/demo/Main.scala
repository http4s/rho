package com.http4s.rho.swagger.demo

import org.http4s.Service
import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import org.http4s.util.StreamApp
import fs2.{Task, Stream}

object Main extends StreamApp {
  private val logger = getLogger

  val port = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def stream(args: List[String]): Stream[Task, Unit] = {
    val middleware = SwaggerSupport()

    BlazeBuilder
      .mountService(Service.withFallback(StaticContentService.routes)(MyService.toService(middleware)))
      .bindLocal(port)
      .serve
  }
}
