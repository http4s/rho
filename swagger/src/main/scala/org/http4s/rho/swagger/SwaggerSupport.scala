package org.http4s
package rho
package swagger

import io.swagger.util.Json

import headers.`Content-Type`
import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.rho.swagger.models.{Swagger, Info}

import shapeless._

object SwaggerSupport {

  /**
   * Create a RhoMiddleware adding a route to get the Swagger json file
   * representing the full API
   */
  def apply(
             swaggerFormats: SwaggerFormats = SwaggerFormats.defaultSwaggerFormats,
             apiPath: TypedPath[HNil] = "swagger.json",
             apiInfo: Info = Info(title = "My API", version = "1.0.0"),
             swaggerRoutesInSwagger: Boolean = false): RhoMiddleware = { routes =>

    lazy val swaggerSpec: Swagger =
      createSwagger(swaggerFormats, apiPath, apiInfo)(
        routes ++ (if(swaggerRoutesInSwagger) swaggerRoute else Seq.empty )
      )

    lazy val swaggerRoute: Seq[RhoRoute[_ <: HList]] =
      createSwaggerRoute(swaggerSpec, apiPath).getRoutes

    routes ++ swaggerRoute
  }

  /**
   * Create the swagger model for a set of routes
   */
  def createSwagger(
                     swaggerFormats: SwaggerFormats = SwaggerFormats.defaultSwaggerFormats,
                     apiPath: TypedPath[HNil] = "swagger.json",
                     apiInfo: Info = Info(title = "My API", version = "1.0.0"))(routes: Seq[RhoRoute[_]]): Swagger = {
    val sb = new SwaggerModelsBuilder(swaggerFormats)
    routes.foldLeft(Swagger())((s, r) => sb.mkSwagger(apiInfo, r)(s))
  }

  /**
   * Create a RhoService with the route to the Swagger json
   * for the given Swagger Specification
   */
  def createSwaggerRoute(
    swagger: => Swagger,
    apiPath: TypedPath[HNil] = "swagger.json"
  ): RhoService = new RhoService {
    lazy val response = Ok(
      Json.mapper()
        .writerWithDefaultPrettyPrinter()
        .writeValueAsString(swagger.toJModel)
    ).putHeaders(`Content-Type`(MediaType.`application/json`))

    "Swagger documentation" ** GET / apiPath |>> (() => response)
  }
}

