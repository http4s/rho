package com.http4s.rho.swagger.demo

import cats.effect.IO
import org.http4s.Service
import org.http4s.rho.swagger.SwaggerSupport
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import org.http4s.util.{ExitCode, StreamApp}
import fs2.Stream
import cats.implicits._
import org.http4s.implicits._

object Main extends StreamApp[IO] {
  private val logger = getLogger

  val port = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    val middleware = SwaggerSupport()

    BlazeBuilder[IO]
      .mountService(MyService.toService(middleware) <+> StaticContentService.routes)
      .bindLocal(port)
      .serve
  }
}
