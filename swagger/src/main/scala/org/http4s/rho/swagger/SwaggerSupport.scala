package org.http4s
package rho
package swagger

import _root_.io.swagger.util.Json
import cats.effect.Sync
import org.http4s.headers.`Content-Type`
import org.http4s.rho.bits.PathAST.{PathMatch, TypedPath}
import org.http4s.rho.swagger.models._
import shapeless._

import scala.reflect.runtime.universe._
import scala.collection.immutable.Seq

object SwaggerSupport {
  def apply[F[_]: Sync](implicit etag: WeakTypeTag[F[_]]): SwaggerSupport[F] = new SwaggerSupport[F] {}
}

abstract class SwaggerSupport[F[_]](implicit F: Sync[F], etag: WeakTypeTag[F[_]]) extends SwaggerSyntax[F] {

  /**
    * Create a RhoMiddleware adding a route to get the Swagger json file
    * representing the full API
    */
  def createRhoMiddleware(
      swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
      apiPath: TypedPath[F, HNil] = TypedPath(PathMatch("swagger.json")),
      apiInfo: Info = Info(title = "My API", version = "1.0.0"),
      swaggerRoutesInSwagger: Boolean = false,
      host: Option[String] = None,
      basePath: Option[String] = None,
      schemes: List[Scheme] = Nil,
      consumes: List[String] = Nil,
      produces: List[String] = Nil,
      security: List[SecurityRequirement] = Nil,
      securityDefinitions: Map[String, SecuritySchemeDefinition] = Map.empty,
      tags: List[Tag] = Nil,
      vendorExtensions: Map[String, AnyRef] = Map.empty): RhoMiddleware[F] = { routes =>

    lazy val swaggerSpec: Swagger =
      createSwagger(swaggerFormats, apiInfo, host, basePath, schemes, consumes, produces, security, securityDefinitions, tags, vendorExtensions)(
        routes ++ (if(swaggerRoutesInSwagger) swaggerRoute else Seq.empty )
      )

    lazy val swaggerRoute: Seq[RhoRoute[F, _ <: HList]] =
      createSwaggerRoute(swaggerSpec, apiPath).getRoutes

    routes ++ swaggerRoute
  }

  /**
    * Create the swagger model for a set of routes.
    */
  def createSwagger(
      swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
      apiInfo: Info = Info(title = "My API", version = "1.0.0"),
      host: Option[String] = None,
      basePath: Option[String] = None,
      schemes: List[Scheme] = Nil,
      consumes: List[String] = Nil,
      produces: List[String] = Nil,
      security: List[SecurityRequirement] = Nil,
      securityDefinitions: Map[String, SecuritySchemeDefinition] = Map.empty,
      tags: List[Tag] = Nil,
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
        tags = tags,
        vendorExtensions = vendorExtensions
      )
  }

  /**
   * Create a RhoRoutes with the route to the Swagger json for the given Swagger Specification.
   */
  def createSwaggerRoute(
    swagger: => Swagger,
    apiPath: TypedPath[F, HNil] = TypedPath(PathMatch("swagger.json"))
  ): RhoRoutes[F] = new RhoRoutes[F] {

    lazy val response: F[OK[String]] = {
      val fOk = Ok.apply(
        Json.mapper()
          .writerWithDefaultPrettyPrinter()
          .writeValueAsString(swagger.toJModel)
      )

      F.map(fOk) { ok =>
        ok.copy(resp = ok.resp.putHeaders(`Content-Type`(MediaType.application.json)))
      }
    }

    "Swagger documentation" ** GET / apiPath |>> (() => response)
  }
}
