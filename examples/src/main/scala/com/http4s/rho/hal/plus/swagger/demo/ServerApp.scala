package com.http4s.rho.hal.plus.swagger.demo

import org.http4s.server.blaze.BlazeBuilder
import net.sf.uadetector.service.UADetectorServiceFactory.ResourceModuleXmlDataStore

import com.http4s.rho.Helpers._

import org.log4s.getLogger

class ServerApp(port: Int) {

  val businessLayer = new UADetectorDatabase(new ResourceModuleXmlDataStore())

  val routes = new Routes(businessLayer)

  val server = BlazeBuilder
    .mountService(routes.staticContent orElse routes.dynamicContent, "")
    .bindLocal(port)

  def run(): Unit = server.start.run.awaitShutdown()
}

object ServerApp {
  private val logger = getLogger

  val port = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Http4s-blaze example on '$port'")
  println(s"Starting Http4s-blaze example on '$port'")

  def main(args: Array[String]): Unit = new ServerApp(port).run()
}
