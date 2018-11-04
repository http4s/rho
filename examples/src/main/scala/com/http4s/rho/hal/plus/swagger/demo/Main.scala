package com.http4s.rho.hal.plus.swagger.demo

import cats.syntax.semigroupk._
import cats.syntax.functor._
import cats.effect.{ExitCode, IO, IOApp}
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import org.http4s.server.blaze.BlazeBuilder
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

    BlazeBuilder[IO]
      .mountService(routes.staticContent combineK routes.dynamicContent, "")
      .bindLocal(port)
      .serve.compile.drain.as(ExitCode.Success)
  }
}
