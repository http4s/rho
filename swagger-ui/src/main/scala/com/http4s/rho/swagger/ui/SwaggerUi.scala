package com.http4s.rho.swagger.ui

import cats.effect.{Blocker, ContextShift, Sync}
import cats.implicits._
import org.http4s.RhoDsl
import org.http4s.rho.bits.PathAST.{PathMatch, TypedPath}
import org.http4s.rho.swagger.models._
import org.http4s.rho.swagger.{SwaggerFormats, SwaggerMetadata, SwaggerSupport, SwaggerSyntax}
import org.http4s.rho.{RhoMiddleware, RhoRoute, swagger}
import shapeless.{HList, HNil}

import scala.collection.immutable.Seq
import scala.reflect.runtime.universe.WeakTypeTag

object SwaggerUi {
  def apply[F[+ _] : Sync : ContextShift](implicit etag: WeakTypeTag[F[_]]): SwaggerUi[F] = new SwaggerUi[F]()
}

class SwaggerUi[F[+ _] : Sync : ContextShift](implicit etag: WeakTypeTag[F[_]]) extends SwaggerSyntax[F] {

  def createRhoMiddleware(
                           blocker: Blocker,
                           swaggerFormats: SwaggerFormats = swagger.DefaultSwaggerFormats,
                           swaggerSpecPath: String = "swagger.json",
                           swaggerUiPath: String = "swagger-ui",
                           swaggerRoutesInSwagger: Boolean = false,
                           swaggerMetadata: SwaggerMetadata = SwaggerMetadata()): RhoMiddleware[F] = { routes: Seq[RhoRoute[F, _ <: HList]] =>

      lazy val swaggerSpec: Swagger =
        SwaggerSupport[F].createSwagger(swaggerFormats, swaggerMetadata)(
          routes ++ (if (swaggerRoutesInSwagger) swaggerSpecRoute else Seq.empty)
        )

      lazy val swaggerSpecRoute: Seq[RhoRoute[F, _ <: HList]] =
        SwaggerSupport[F].createSwaggerRoute(swaggerSpec, TypedPath(PathMatch(swaggerSpecPath))).getRoutes

      val cleanSwaggerSpecPath = swaggerSpecPath.stripPrefix("/").stripSuffix("/")
      val cleanSwaggerUiPath = swaggerUiPath.stripPrefix("/").stripSuffix("/")
      val relativeSwaggerSpecPath = ("../" * s"$cleanSwaggerUiPath/index.html".count(_ =='/')) + cleanSwaggerSpecPath

      val swaggerUiRoutes = SwaggerUiRoutes[F](relativeSwaggerSpecPath, blocker)
      val swaggerUiTypedPath = typedPath(swaggerUiPath)
      val swaggerUi = (swaggerUiTypedPath /: swaggerUiRoutes).getRoutes

      routes ++ swaggerSpecRoute ++ swaggerUi
    }

  private def typedPath(s: String): TypedPath[F, HNil] =
    s.stripPrefix("/").stripSuffix("/").split("/")
      .foldLeft(RhoDsl[F].root())(_ / _)
}
