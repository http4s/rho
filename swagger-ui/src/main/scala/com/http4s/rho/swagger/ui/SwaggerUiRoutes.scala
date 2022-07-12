package com.http4s.rho.swagger.ui

import cats.effect.Sync
import cats.implicits._
import org.http4s.headers.`Content-Type`
import org.http4s.rho.RhoRoutes
import org.http4s.rho.bits.PathAST.CaptureTail
import org.http4s.rho.swagger.ui.BuildInfo
import org.http4s._

class SwaggerUiRoutes[F[_]: Sync](
    swaggerUiPath: String,
    swaggerUiResourcesPath: String,
    indexHtml: String)
    extends RhoRoutes[F] {

  private val htmlEncoder: EntityEncoder[F, String] =
    EntityEncoder
      .stringEncoder()
      .withContentType(`Content-Type`(MediaType.text.html).withCharset(org.http4s.Charset.`UTF-8`))

  // Serving the html directly here would break all relative paths, so we redirect.
  GET / swaggerUiPath |>> { req: Request[F] =>
    PermanentRedirect(req.uri / "")
  }

  // The "" is the path that we normally want to use. The "index.html" is here to hide the "index.html" from the webjar.
  GET / swaggerUiPath / ("" || "index.html") |>> { () =>
    Ok(indexHtml)(implicitly, htmlEncoder)
  }

  GET / swaggerUiPath / CaptureTail |>> { (req: Request[F], path: List[String]) =>
    fetchResource(swaggerUiResourcesPath + path.mkString("/", "/", ""), req)
  }

  private def fetchResource(path: String, req: Request[F]): F[Response[F]] =
    StaticFile.fromResource[F](path, Some(req)).getOrElseF(NotFound(()).map(_.resp))

}

object SwaggerUiRoutes {

  def apply[F[_]: Sync](
      swaggerUiPath: String,
      swaggerSpecRelativePath: String): SwaggerUiRoutes[F] = {
    val swaggerUiResourcesPath =
      s"/META-INF/resources/webjars/swagger-ui/${BuildInfo.swaggerUiVersion}/"
    val indexHtml = defaultIndexHtml(swaggerSpecRelativePath)
    new SwaggerUiRoutes[F](swaggerUiPath, swaggerUiResourcesPath, indexHtml)
  }

  def defaultIndexHtml(swaggerUrl: String): String =
    s"""
       |<!-- HTML for static distribution bundle build -->
       |<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |    <meta charset="UTF-8">
       |    <title>Swagger UI</title>
       |    <link rel="stylesheet" type="text/css" href="./swagger-ui.css" >
       |    <link rel="icon" type="image/png" href="./favicon-32x32.png" sizes="32x32" />
       |    <link rel="icon" type="image/png" href="./favicon-16x16.png" sizes="16x16" />
       |    <style>
       |      html
       |      {
       |        box-sizing: border-box;
       |        overflow: -moz-scrollbars-vertical;
       |        overflow-y: scroll;
       |      }
       |
       |      *,
       |      *:before,
       |      *:after
       |      {
       |        box-sizing: inherit;
       |      }
       |
       |      body
       |      {
       |        margin:0;
       |        background: #fafafa;
       |      }
       |    </style>
       |</head>
       |
       |<body>
       |<div id="swagger-ui"></div>
       |
       |<script src="./swagger-ui-bundle.js"> </script>
       |<script src="./swagger-ui-standalone-preset.js"> </script>
       |<script>
       |    window.onload = function() {
       |      // Begin Swagger UI call region
       |      const ui = SwaggerUIBundle({
       |        url: "$swaggerUrl",
       |        dom_id: '#swagger-ui',
       |        deepLinking: true,
       |        presets: [
       |          SwaggerUIBundle.presets.apis,
       |          SwaggerUIStandalonePreset
       |        ],
       |        plugins: [
       |          SwaggerUIBundle.plugins.DownloadUrl
       |        ],
       |        layout: "StandaloneLayout"
       |      })
       |      // End Swagger UI call region
       |
       |      window.ui = ui
       |    }
       |  </script>
       |</body>
       |</html>
       |""".stripMargin
}
