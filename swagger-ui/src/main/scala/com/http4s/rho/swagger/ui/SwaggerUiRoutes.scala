package com.http4s.rho.swagger.ui

import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import com.http4s.rho.swagger.ui.SwaggerUiRoutes.Html
import org.http4s.headers.`Content-Type`
import org.http4s.rho.RhoRoutes
import org.http4s.rho.bits.PathAST.CaptureTail
import org.http4s.{EntityEncoder, MediaType, Request, Response, StaticFile}
import org.webjars.WebJarAssetLocator

class SwaggerUiRoutes[F[+ _] : Sync : ContextShift](swaggerUiResourcesPath: String,
                                                    indexHtml: Html,
                                                    blocker: Blocker) extends RhoRoutes[F] {

  private implicit val htmlEncoder: EntityEncoder[F, Html] =
    EntityEncoder.stringEncoder[F]
      .contramap[Html](html => html.html)
      .withContentType(`Content-Type`(MediaType.text.html).withCharset(org.http4s.Charset.`UTF-8`))

  GET |>> { req: Request[F] =>
    PermanentRedirect(req.uri / "index.html")
  }

  GET / "index.html" |>> { () =>
    indexHtml
  }

  GET / CaptureTail |>> { (req: Request[F], path: List[String]) =>
    fetchResource(swaggerUiResourcesPath + path.mkString("/", "/", ""), req)
  }

  private def fetchResource(path: String, req: Request[F]): F[Response[F]] =
    StaticFile.fromResource[F](path, blocker, Some(req)).getOrElseF(NotFound(()).map(_.resp))

}

object SwaggerUiRoutes {

  def apply[F[+ _] : Sync : ContextShift](swaggerUrl: String, blocker: Blocker): F[SwaggerUiRoutes[F]] = for {

    swaggerUiResourcesPath <- discoverPathToSwaggerUiResources[F](blocker)
    indexHtml = defaultIndexHtml(swaggerUrl)
  } yield new SwaggerUiRoutes[F](swaggerUiResourcesPath, indexHtml, blocker)

  def discoverPathToSwaggerUiResources[F[+_] : Sync : ContextShift](blocker: Blocker): F[String] = for {
    swaggerUiVersion <- blocker.delay(new WebJarAssetLocator().getWebJars.get("swagger-ui"))
  } yield s"/META-INF/resources/webjars/swagger-ui/$swaggerUiVersion/"

  case class Html(html: String)

  def defaultIndexHtml(swaggerUrl: String): Html = Html(
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
       |""".stripMargin)
}
