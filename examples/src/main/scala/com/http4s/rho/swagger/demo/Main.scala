package com.http4s.rho.swagger.demo

import cats.effect.IO
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import org.http4s.HttpRoutes
import cats.syntax.semigroupk._
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.rho.swagger.syntax.io._
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StreamApp[IO] {
  private val logger = getLogger

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    val middleware = createRhoMiddleware()

    val myService: HttpRoutes[IO] =
      new MyService[IO](ioSwagger) {}.toRoutes(middleware)

    BlazeBuilder[IO]
      .mountService((StaticContentService.routes <+> myService))
      .bindLocal(port)
      .serve
  }
}
