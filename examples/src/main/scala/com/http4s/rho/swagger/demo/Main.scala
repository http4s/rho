package com.http4s.rho.swagger.demo

import cats.effect.IO
import cats.syntax.semigroupk._
import fs2.Stream
import org.http4s.HttpService
import org.http4s.rho.swagger.ioSyntax
import org.http4s.rho.swagger.ioSyntax._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.util.{ExitCode, StreamApp}
import org.log4s.getLogger

object Main extends StreamApp[IO] {
  private val logger = getLogger

  val port = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def stream(args: List[String]): Stream[IO, ExitCode] = {
    val middleware = createRhoMiddleware()
    val myService: HttpService[IO] =
      new MyService[IO](org.http4s.rho.apply[IO], ioSyntax) {}.toService(middleware)

    BlazeBuilder[IO]
      .mountService(StaticContentService.routes combineK myService)
      .bindLocal(port)
      .serve
  }
}
