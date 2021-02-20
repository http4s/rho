package com.http4s.rho.hal.plus.swagger.demo

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import cats.implicits._
import org.http4s.implicits._
import org.http4s.server.blaze._
import org.log4s.getLogger

import scala.concurrent.ExecutionContext.global

object Main extends IOApp {
  private val logger = getLogger

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Hal example on '$port'")

  def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { blocker =>
      val businessLayer = new UADetectorDatabase(new ResourceModuleXmlDataStore())

      val routes =
        new Routes(businessLayer, blocker)

      BlazeServerBuilder[IO](global)
        .withHttpApp((routes.staticContent <+> routes.dynamicContent).orNotFound)
        .bindLocal(port)
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
}
