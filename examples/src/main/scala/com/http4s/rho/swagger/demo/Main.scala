package com.http4s.rho.swagger.demo

import java.util.concurrent.Executors

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.implicits._
import org.http4s.implicits._
import org.http4s.HttpRoutes
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.http4s.server.blaze.BlazeServerBuilder
import org.log4s.getLogger

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  private val logger = getLogger
  import ioSwagger._

  val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] = {
    val blockingExecutor = Executors.newCachedThreadPool()
    implicit val blocker: Blocker = Blocker.liftExecutionContext(ExecutionContext.fromExecutor(blockingExecutor))

    val middleware = createRhoMiddleware()

    val myService: HttpRoutes[IO] =
      new MyRoutes[IO](ioSwagger).toRoutes(middleware)

    BlazeServerBuilder[IO]
      .withHttpApp((StaticContentService.routes <+> myService).orNotFound)
      .bindLocal(port)
      .serve.compile.drain.as(ExitCode.Success)
  }
}
