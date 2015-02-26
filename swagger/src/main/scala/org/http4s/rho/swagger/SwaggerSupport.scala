package org.http4s
package rho
package swagger

import com.wordnik.swagger.{ models => m }
import com.wordnik.swagger.util.Json

import headers.`Content-Type`

import shapeless.HList

trait SwaggerSupport extends RhoService {
  import models._

  var swagger: Swagger = Swagger()

  /** Override the `swaggerFormats` to add your own custom serializers */
  def swaggerFormats: SwaggerFormats = DefaultSwaggerFormats

  def apiPath = "swagger.json"

  def apiInfo: Info = Info(title = "My API", version = "1.0.0")

  GET / apiPath |>> { () =>
    val swaggerJson = Json.mapper().writeValueAsString(swagger.toJModel)
    Ok(swaggerJson).withHeaders(`Content-Type`(MediaType.`application/json`))
  }

  override protected def append[T <: HList, F](ra: RhoAction[T, F]): Unit = {
    super.append(ra)
    val sb = new SwaggerModelsBuilder(swaggerFormats)    
    swagger = sb.mkSwagger(apiInfo, ra)(swagger)
  }
}
