package com.http4s.rho.swagger.demo

import cats.effect.{ExitCode, IO, IOApp}
import com.http4s.rho.swagger.ui.SwaggerUi
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.rho.swagger.SwaggerMetadata
import org.http4s.rho.swagger.models.{Info, Tag}
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.log4s.getLogger

object Main extends IOApp {
  private val logger = getLogger

  private val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] = {
    val metadata = SwaggerMetadata(
      apiInfo = Info(title = "Rho demo", version = "1.2.3"),
      tags = List(Tag(name = "hello", description = Some("These are the hello routes.")))
    )

    val swaggerUiRhoMiddleware =
      SwaggerUi[IO].createRhoMiddleware(swaggerMetadata = metadata)
    val myRoutes = new MyRoutes[IO](ioSwagger).toRoutes(swaggerUiRhoMiddleware)

    BlazeServerBuilder[IO]
      .withHttpApp(myRoutes.orNotFound)
      .bindLocal(port)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
