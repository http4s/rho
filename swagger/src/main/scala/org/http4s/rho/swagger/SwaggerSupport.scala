package org.http4s
package rho
package swagger

import io.swagger.util.Json

import headers.`Content-Type`
import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.rho.swagger.models.{Swagger, Info}

import shapeless.HNil

object SwaggerSupport {
  def apply(
    swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
    apiPath: TypedPath[HNil] = "swagger.json",
    apiInfo: Info = Info(title = "My API", version = "1.0.0")): RhoMiddleware = { routes =>
    val sb = new SwaggerModelsBuilder(swaggerFormats)
    var swaggerSpec: Swagger = Swagger()

    val swaggerRoutes = new RhoService {
      lazy val swaggerResponse =
      Ok(Json.mapper().writeValueAsString(swaggerSpec.toJModel))
        .putHeaders(`Content-Type`(MediaType.`application/json`))

      "Swagger documentation" ** GET / apiPath |>> { () => swaggerResponse }
    }.getRoutes()

    val rs = routes ++ swaggerRoutes
    swaggerSpec = rs.foldLeft(swaggerSpec) { (s, r) => sb.mkSwagger(apiInfo, r)(s) }

    rs
  }
}

