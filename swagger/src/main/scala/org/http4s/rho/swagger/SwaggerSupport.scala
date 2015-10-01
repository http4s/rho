package org.http4s
package rho
package swagger

import io.swagger.util.Json

import headers.`Content-Type`
import org.http4s.rho.bits.PathAST.TypedPath

import shapeless.{HNil, HList}

trait SwaggerSupport extends RhoService {
  import models._

  private var swaggerSpec: Swagger = Swagger()
  private lazy val swaggerResponse = Json.mapper().writeValueAsString(swaggerSpec.toJModel)

  /** Override the `swaggerFormats` to add your own custom serializers */
  def swaggerFormats: SwaggerFormats = DefaultSwaggerFormats

  def apiPath: TypedPath[HNil] = "swagger.json"

  def apiInfo: Info = Info(title = "My API", version = "1.0.0")

  "Swagger documentation" **
    GET / apiPath |>> { () => Ok(swaggerResponse).putHeaders(`Content-Type`(MediaType.`application/json`)) }

  override protected def append[T <: HList](rr: RhoRoute[T]): Unit = {
    super.append(rr)
    val sb = new SwaggerModelsBuilder(swaggerFormats)    
    swaggerSpec = sb.mkSwagger(apiInfo, rr)(swaggerSpec)
  }
}
