package com.http4s.rho.hal.plus.swagger.demo

import cats.effect.IO
import cats.syntax.semigroupk._
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StreamApp[IO] {
  private val logger = getLogger

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Hal example on '$port'")

  def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    val businessLayer =
      new UADetectorDatabase(new ResourceModuleXmlDataStore())

    val routes =
      new Routes(businessLayer)

    BlazeBuilder[IO]
      .mountService(routes.staticContent combineK routes.dynamicContent, "")
      .bindLocal(port)
      .serve
  }
}
