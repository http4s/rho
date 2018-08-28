package com.http4s.rho.swagger.demo

import cats.effect.IO
import org.http4s.{HttpRoutes, Request, Response, StaticFile}
import org.http4s.dsl.io._

object StaticContentService {
  private val swaggerUiDir = "/swagger-ui"

  def fetchResource(path: String, req: Request[IO]): IO[Response[IO]] = {
    StaticFile.fromResource(path, Some(req)).getOrElseF(NotFound())
  }

  /**
   * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
   * but its nice to keep it self contained
   */
  def routes: HttpRoutes[IO] = HttpRoutes.of {
    // Swagger User Interface
    case req @ GET -> Root / "css" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "images" / _    => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "lib" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "swagger-ui"    => fetchResource(swaggerUiDir + "/index.html", req)
    case req @ GET -> Root / "swagger-ui.js" => fetchResource(swaggerUiDir + "/swagger-ui.min.js", req)
  }
}
