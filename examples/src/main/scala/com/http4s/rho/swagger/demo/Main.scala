package com.http4s.rho.swagger.demo

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.HttpRoutes
import org.http4s.rho.swagger.syntax.io._
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import cats.syntax.semigroupk._

object Main extends IOApp {
  private val logger = getLogger

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] = {
    val middleware = createRhoMiddleware()

    val myService: HttpRoutes[IO] =
      new MyService[IO](ioSwagger) {}.toRoutes(middleware)

    BlazeBuilder[IO]
      .mountService((StaticContentService.routes <+> myService))
      .bindLocal(port)
      .serve.compile.toList.map(_.head)
  }
}
