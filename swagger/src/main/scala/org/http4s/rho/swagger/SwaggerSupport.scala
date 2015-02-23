package org.http4s
package rho
package swagger

import com.wordnik.swagger.{ models => m }
import com.wordnik.swagger.util.Json

import headers.`Content-Type`

import shapeless.HList

trait SwaggerSupport extends RhoService {
  import models._

  private var swagger: Option[Swagger] = None

  /** Override the `swaggerFormats` to add your own custom serializers */
  def swaggerFormats: SwaggerFormats = DefaultSwaggerFormats

  def apiPath = "swagger.json"

  def apiInfo: Info = Info(title = "My API", version = "1.0.0")

  GET / apiPath |>> { () =>
    val swaggerJson = Json.mapper().writeValueAsString(swagger.map(_.toJModel).get)
    Ok(swaggerJson).withHeaders(`Content-Type`(MediaType.`application/json`))
  }

  override protected def append[T <: HList, F](ra: RhoAction[T, F]): Unit = {
    super.append(ra)
    val sb = new SwaggerModelsBuilder(swaggerFormats)
    val s = sb.mkSwagger(apiInfo, ra)
    swagger = swagger.map(merge(_, s)).orElse(Some(s))
  }

  private def merge(s1: Swagger, s2: Swagger): Swagger =
    Swagger(
      info        = s2.info,
      paths       = s1.paths ++ s2.paths,
      definitions = s1.definitions ++ s2.definitions)
}
