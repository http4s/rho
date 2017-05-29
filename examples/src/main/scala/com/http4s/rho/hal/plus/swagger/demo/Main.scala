package com.http4s.rho.hal.plus.swagger.demo

import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore
import org.http4s.Service
import org.http4s.server.blaze.BlazeBuilder
import org.log4s.getLogger
import fs2.{Task, Stream}
import org.http4s.util.StreamApp


object Main extends StreamApp {
  private val logger = getLogger

  val port = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Hal example on '$port'")

  def stream(args: List[String]): Stream[Task, Nothing] = {
    val businessLayer = new UADetectorDatabase(new ResourceModuleXmlDataStore())
    val routes = new Routes(businessLayer)

    BlazeBuilder
      .mountService(Service.withFallback(routes.staticContent)(routes.dynamicContent), "")
      .bindLocal(port)
      .serve
  }
}
