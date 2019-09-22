package com.http4s.rho.hal.plus.swagger.demo

import cats.data.OptionT
import cats.effect.{Blocker, ContextShift, Sync, Timer}
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpRoutes, Request, Response, StaticFile}

class StaticContentService[F[_]: Sync : Timer : ContextShift](dsl: Http4sDsl[F], blocker: Blocker) {
  import dsl._

  private val halUiDir = "/hal-browser"
  private val swaggerUiDir = "/swagger-ui"

  /**
   * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
   * but its nice to keep it self contained.
   */
  def routes: HttpRoutes[F] = HttpRoutes[F] {

    // JSON HAL User Interface
    case req if req.uri.path.startsWith("/js/") => fetchResource(halUiDir + req.pathInfo, req)
    case req if req.uri.path.startsWith("/vendor/") => fetchResource(halUiDir + req.pathInfo, req)
    case req @ GET -> Root / "hal-ui" => fetchResource(halUiDir + "/browser.html", req)

    // Swagger User Interface
    case req @ GET -> Root / "css" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "images" / _    => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "lib" / _       => fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "swagger-ui"    => fetchResource(swaggerUiDir + "/index.html", req)
    case req @ GET -> Root / "swagger-ui.js" => fetchResource(swaggerUiDir + "/swagger-ui.min.js", req)
  }

  private def fetchResource(path: String, req: Request[F]): OptionT[F, Response[F]] = {
    StaticFile.fromResource(path, blocker, Some(req))
  }
}
