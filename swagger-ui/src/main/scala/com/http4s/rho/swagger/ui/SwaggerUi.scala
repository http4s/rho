package com.http4s.rho.swagger.ui

import cats.effect.Sync
import org.http4s.rho.bits.PathAST.{PathMatch, TypedPath}
import org.http4s.rho.swagger.models._
import org.http4s.rho.swagger.{
  DefaultShowType,
  ShowType,
  SwaggerFormats,
  SwaggerMetadata,
  SwaggerSupport,
  SwaggerSyntax
}
import org.http4s.rho.{RhoMiddleware, RhoRoute, swagger}
import shapeless.HList

import scala.collection.immutable.Seq
import scala.reflect.runtime.universe.WeakTypeTag

object SwaggerUi {
  def apply[F[_]: Sync](implicit etag: WeakTypeTag[F[_]]): SwaggerUi[F] = new SwaggerUi[F]()
}

class SwaggerUi[F[_]: Sync](implicit etag: WeakTypeTag[F[_]]) extends SwaggerSyntax[F] {

  def createRhoMiddleware(
      swaggerFormats: SwaggerFormats = swagger.DefaultSwaggerFormats,
      swaggerSpecJsonPath: String = "swagger.json",
      swaggerSpecYamlPath: String = "swagger.yaml",
      useYamlInSwaggerUi: Boolean = false,
      swaggerUiPath: String = "swagger-ui",
      swaggerRoutesInSwagger: Boolean = false,
      swaggerMetadata: SwaggerMetadata = SwaggerMetadata(),
      showType: ShowType = DefaultShowType): RhoMiddleware[F] = {
    routes: Seq[RhoRoute[F, _ <: HList]] =>
      lazy val swaggerSpec: Swagger =
        SwaggerSupport[F].createSwagger(swaggerFormats, swaggerMetadata, showType)(
          routes ++ (if (swaggerRoutesInSwagger) swaggerSpecRoute else Seq.empty)
        )

      lazy val swaggerSpecRoute: Seq[RhoRoute[F, _ <: HList]] =
        SwaggerSupport[F]
          .createSwaggerRoute(
            swaggerSpec,
            TypedPath(PathMatch(swaggerSpecJsonPath)),
            TypedPath(PathMatch(swaggerSpecYamlPath))
          )
          .getRoutes

      val cleanSwaggerSpecPath = (if (useYamlInSwaggerUi) swaggerSpecYamlPath
                                  else swaggerSpecJsonPath).stripPrefix("/").stripSuffix("/")
      val cleanSwaggerUiPath = swaggerUiPath.stripPrefix("/").stripSuffix("/")
      val relativeSwaggerSpecPath =
        ("../" * s"$cleanSwaggerUiPath/".count(_ == '/')) + cleanSwaggerSpecPath

      val swaggerUiRoutes = SwaggerUiRoutes[F](swaggerUiPath, relativeSwaggerSpecPath).getRoutes

      routes ++ swaggerSpecRoute ++ swaggerUiRoutes
  }
}
