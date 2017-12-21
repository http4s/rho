package org.http4s
package rho
package swagger

import _root_.io.swagger.util.Json
import cats.Monad
import org.http4s.headers.`Content-Type`
import org.http4s.rho.bits.PathAST.TypedPath
import org.http4s.rho.swagger.models._
import shapeless._

object SwaggerSupport {
  def apply[F[_]: Monad](dsl: RhoDsl[F], syntax: SwaggerSyntax[F]): SwaggerSupport[F] =
    new SwaggerSupport[F](dsl) {}
}

abstract class SwaggerSupport[F[_]](dsl: RhoDsl[F])(implicit F: Monad[F]) extends SwaggerSyntax[F] {
  import dsl._

  /**
    * Create a RhoMiddleware adding a route to get the Swagger json file
    * representing the full API
    */
  def createRhoMiddleware(
      swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
      apiPath: TypedPath[F, HNil] = "swagger.json",
      apiInfo: Info = Info(title = "My API", version = "1.0.0"),
      swaggerRoutesInSwagger: Boolean = false,
      host: Option[String] = None,
      basePath: Option[String] = None,
      schemes: List[Scheme] = Nil,
      consumes: List[String] = Nil,
      produces: List[String] = Nil,
      security: List[SecurityRequirement] = Nil,
      securityDefinitions: Map[String, SecuritySchemeDefinition] = Map.empty,
      vendorExtensions: Map[String, AnyRef] = Map.empty): RhoMiddleware[F] = { routes =>

    lazy val swaggerSpec: Swagger =
      createSwagger(swaggerFormats, apiPath, apiInfo, host, basePath, schemes, consumes, produces, security, securityDefinitions, vendorExtensions)(
        routes ++ (if(swaggerRoutesInSwagger) swaggerRoute else Seq.empty )
      )

    lazy val swaggerRoute: Seq[RhoRoute[F, _ <: HList]] =
      createSwaggerRoute(swaggerSpec, apiPath).getRoutes

    routes ++ swaggerRoute
  }

  /**
    * Create the swagger model for a set of routes
    */
  def createSwagger(
      swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
      apiPath: TypedPath[F, HNil] = "swagger.json",
      apiInfo: Info = Info(title = "My API", version = "1.0.0"),
      host: Option[String] = None,
      basePath: Option[String] = None,
      schemes: List[Scheme] = Nil,
      consumes: List[String] = Nil,
      produces: List[String] = Nil,
      security: List[SecurityRequirement] = Nil,
      securityDefinitions: Map[String, SecuritySchemeDefinition] = Map.empty,
      vendorExtensions: Map[String, AnyRef] = Map.empty)(routes: Seq[RhoRoute[F, _]]): Swagger = {

    val sb = new SwaggerModelsBuilder(swaggerFormats)
    routes.foldLeft(Swagger())((s, r) => sb.mkSwagger(apiInfo, r)(s))
      .copy(
        host = host,
        basePath = basePath,
        schemes = schemes,
        consumes = consumes,
        produces = produces,
        security = security,
        securityDefinitions = securityDefinitions,
        vendorExtensions = vendorExtensions
      )
  }

  /**
   * Create a RhoService with the route to the Swagger json
   * for the given Swagger Specification
   */
  def createSwaggerRoute(
    swagger: => Swagger,
    apiPath: TypedPath[F, HNil] = "swagger.json"
  ): RhoService[F] = new RhoService[F] {

    lazy val response: F[OK[F, String]] = {
      val fOk = Ok[F].apply(
        Json.mapper()
          .writerWithDefaultPrettyPrinter()
          .writeValueAsString(swagger.toJModel)
      )

      F.map(fOk) { ok =>
        ok.copy(resp = ok.resp.putHeaders(`Content-Type`(MediaType.`application/json`)))
      }
    }

    "Swagger documentation" ** GET / apiPath |>> (() => response)
  }
}
