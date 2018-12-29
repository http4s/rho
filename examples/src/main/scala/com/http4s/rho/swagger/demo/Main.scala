package com.http4s.rho.swagger.demo

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import org.http4s.HttpRoutes
import org.http4s.syntax.all._
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.server.blaze.BlazeServerBuilder
import org.log4s.getLogger

object Main extends IOApp {
  private val logger = getLogger
  import ioSwagger._

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] = {
    val middleware = createRhoMiddleware()

    val myAuthedRoute: MyAuthedRoutes[IO] = new MyAuthedRoutes[IO]()

    val myRoutes = new MyRoutes[IO](ioSwagger) {}

    val myRhoServices = (
      (myAuthedRoute.toRhoRoutes(ExampleAuth.simpleAuthMiddlware)) and
        myRoutes
      )

    val myService: HttpRoutes[IO] = myRhoServices.toHttpRoutes(middleware)

    BlazeServerBuilder[IO]
      .withHttpApp((StaticContentService.routes <+> myService).orNotFound)
      .bindLocal(port)
      .serve.compile.drain.as(ExitCode.Success)
  }
}
