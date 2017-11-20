package com.http4s.rho.hal.plus.swagger.demo

import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import org.http4s.{Request, Response, Service}
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import fs2.Stream
import org.http4s.util.{ExitCode, StreamApp}
import cats.effect._
import cats.implicits._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dsl.io._


object Main extends StreamApp[IO] {
  private val logger = getLogger

  val port = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Hal example on '$port'")

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    val businessLayer = new UADetectorDatabase(new ResourceModuleXmlDataStore())
    val routes = new Routes(businessLayer)

    BlazeBuilder[IO]
      .mountService(routes.dynamicContent <+> routes.staticContent, "")
      .bindLocal(port)
      .serve
  }
}

