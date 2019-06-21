package com.http4s.rho.hal.plus.swagger.demo

import cats.effect.{ExitCode, IO, IOApp}
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.log4s.getLogger

object Main extends IOApp {
  private val logger = getLogger

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Hal example on '$port'")

  def run(args: List[String]): IO[ExitCode] = {
    val businessLayer = new UADetectorDatabase(new ResourceModuleXmlDataStore())

    val routes =
      new Routes(businessLayer)

    BlazeServerBuilder[IO]
      .withHttpApp((routes.staticContent <+> routes.dynamicContent).orNotFound)
      .bindLocal(port)
      .serve.compile.drain.as(ExitCode.Success)
  }
}
