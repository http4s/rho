package com.http4s.rho.hal.plus.swagger.demo

import org.http4s.server.blaze.BlazeServer
import com.typesafe.scalalogging.slf4j.StrictLogging
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore

class ServerApp(port: Int) extends StrictLogging {

  val businessLayer = new UADetectorDatabase(new ResourceModuleXmlDataStore())

  val routes = new Routes(businessLayer)

  val server = BlazeServer.newBuilder
    .mountService(routes.staticContent, "")
    .mountService(routes.dynamicContent, "")
    .withPort(port)
    .build

  def run(): Unit = server.run
}

object ServerApp extends StrictLogging {
  val port = (Option(System.getenv("DATAMEER_REST_PORT")) orElse
    Option(System.getenv("HTTP_PORT")))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Http4s-blaze example on '$port'")
  println(s"Starting Http4s-blaze example on '$port'")

  def main(args: Array[String]): Unit = new ServerApp(port).run()
}
