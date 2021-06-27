package org.http4s
package rho
package swagger

import _root_.io.swagger.util.{Json, Yaml}
import cats._
import org.http4s.headers.`Content-Type`
import org.http4s.rho.bits.PathAST.{PathMatch, TypedPath}
import org.http4s.rho.swagger.models._
import shapeless._

import scala.reflect.runtime.universe._
import scala.collection.immutable.Seq

object SwaggerSupport {
  def apply[F[_]: Monad](implicit etag: WeakTypeTag[F[_]]): SwaggerSupport[F] =
    new SwaggerSupport[F] {}
}

abstract class SwaggerSupport[F[_]: Monad](implicit etag: WeakTypeTag[F[_]])
    extends SwaggerSyntax[F] {

  /** Create a RhoMiddleware adding a route to get the Swagger JSON/YAML files
    * representing the full API
    */
  def createRhoMiddleware(
      swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
      jsonApiPath: TypedPath[F, HNil] = TypedPath(PathMatch("swagger.json")),
      yamlApiPath: TypedPath[F, HNil] = TypedPath(PathMatch("swagger.yaml")),
      swaggerRoutesInSwagger: Boolean = false,
      swaggerMetadata: SwaggerMetadata = SwaggerMetadata(),
      showType: ShowType = DefaultShowType): RhoMiddleware[F] = { routes =>
    lazy val swaggerSpec: Swagger =
      createSwagger(swaggerFormats, swaggerMetadata, showType)(
        routes ++ (if (swaggerRoutesInSwagger) swaggerRoute else Seq.empty)
      )

    lazy val swaggerRoute: Seq[RhoRoute[F, _ <: HList]] =
      createSwaggerRoute(swaggerSpec, jsonApiPath, yamlApiPath).getRoutes

    routes ++ swaggerRoute
  }

  /** Create the swagger model for a set of routes.
    */
  def createSwagger(
      swaggerFormats: SwaggerFormats = DefaultSwaggerFormats,
      swaggerMetadata: SwaggerMetadata = SwaggerMetadata(),
      showType: ShowType = DefaultShowType)(routes: Seq[RhoRoute[F, _]]): Swagger = {

    val sb = new SwaggerModelsBuilder[F](swaggerFormats)(showType, etag)
    routes.foldLeft(swaggerMetadata.toSwagger())((s, r) => sb.mkSwagger(r)(s))
  }

  /** Create a RhoRoutes with the route to the Swagger json for the given Swagger Specification.
    */
  def createSwaggerRoute(
      swagger: => Swagger,
      jsonApiPath: TypedPath[F, HNil] = TypedPath(PathMatch("swagger.json")),
      yamlApiPath: TypedPath[F, HNil] = TypedPath(PathMatch("swagger.yaml"))
  ): RhoRoutes[F] = new RhoRoutes[F] {

    "Swagger documentation (JSON)" ** GET / jsonApiPath |>> (() => jsonResponse)

    lazy val jsonResponse: F[OK[String]] =
      Ok.apply(
        Json
          .mapper()
          .writerWithDefaultPrettyPrinter()
          .writeValueAsString(swagger.toJModel),
        Headers(`Content-Type`(MediaType.application.json))
      )

    "Swagger documentation (YAML)" ** GET / yamlApiPath |>> (() => yamlResponse)

    lazy val yamlResponse: F[OK[String]] =
      Ok.apply(
        Yaml
          .mapper()
          .writerWithDefaultPrettyPrinter()
          .writeValueAsString(swagger.toJModel),
        Headers(`Content-Type`(MediaType.text.yaml))
      )
  }
}
