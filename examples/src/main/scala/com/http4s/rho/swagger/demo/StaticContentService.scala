package com.http4s.rho.swagger.demo

import cats.effect.{IO}
import org.http4s.Uri.Path.Segment
import org.http4s.dsl.io._
import org.http4s.rho.RhoRoutes
import org.http4s.{HttpRoutes, Request, Response, StaticFile}

object StaticContentService {
  private val swaggerUiDir = "/swagger-ui"

  /** Routes for getting static resources. These might be served more efficiently by apache2 or nginx,
    * but its nice to keep it self contained.
    */
  def routes: HttpRoutes[IO] = new RhoRoutes[IO] {
    // Swagger User Interface
    GET / "css" / * |>> { (req: Request[IO], _: List[Segment]) =>
      fetchResource(swaggerUiDir + req.pathInfo, req)
    }
    GET / "images" / * |>> { (req: Request[IO], _: List[Segment]) =>
      fetchResource(swaggerUiDir + req.pathInfo, req)
    }
    GET / "lib" / * |>> { (req: Request[IO], _: List[Segment]) =>
      fetchResource(swaggerUiDir + req.pathInfo, req)
    }
    GET / "swagger-ui" |>> { req: Request[IO] =>
      fetchResource(swaggerUiDir + "/index.html", req)
    }
    GET / "swagger-ui.js" |>> { req: Request[IO] =>
      fetchResource(swaggerUiDir + "/swagger-ui.min.js", req)
    }
  }.toRoutes()

  private def fetchResource(path: String, req: Request[IO]): IO[Response[IO]] =
    StaticFile.fromResource(path, Some(req)).getOrElseF(NotFound())

}
