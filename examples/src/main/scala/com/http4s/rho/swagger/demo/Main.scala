package com.http4s.rho.swagger.demo

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.implicits._
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.server.blaze.BlazeServerBuilder
import org.log4s.getLogger

object Main extends IOApp {
  private val logger = getLogger

  import ioSwagger._

  private val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { blocker =>

      val middleware = createRhoMiddleware()

      val myService: HttpRoutes[IO] =
        new MyRoutes[IO](ioSwagger).toRoutes(middleware)

      BlazeServerBuilder[IO]
        .withHttpApp((StaticContentService.routes(blocker) <+> myService).orNotFound)
        .bindLocal(port)
        .serve.compile.drain.as(ExitCode.Success)
    }
}
