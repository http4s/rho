package com.http4s.rho.swagger.demo

import scala.concurrent.ExecutionContext
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.dsl._
import org.http4s.server.HttpService
import scalaz.concurrent.Task

object StaticContentService {

  private val swaggerUiDir = "/swagger-ui"

  def fetchResource(path: String, req: Request): Task[Response] = {
    StaticFile.fromResource(path, Some(req))
      .map(Task.now)
      .getOrElse(NotFound())
  }

  /**
   * Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
   * but its nice to keep it self contained
   */
  def routes(implicit executionContext: ExecutionContext = ExecutionContext.global): HttpService = HttpService {
    // Swagger User Interface
    case req @ GET -> Root / "css" / rest =>
      fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "images" / rest =>
      fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "lib" / rest =>
      fetchResource(swaggerUiDir + req.pathInfo, req)
    case req @ GET -> Root / "swagger-ui" =>
      fetchResource(swaggerUiDir + "/index.html", req)
    case req @ GET -> Root / "swagger-ui.js" =>
      fetchResource(swaggerUiDir + "/swagger-ui.min.js", req)
  }

}
