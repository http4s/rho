package org.http4s
package rho
package swagger


import com.wordnik.swagger.{ models => m }
import com.wordnik.swagger.util.Json

import headers.`Content-Type`

import shapeless.HList


trait SwaggerSupport extends RhoService {

  /** Override the `swaggerFormats` to add your own custom serializers */
//  def swaggerFormats: SwaggerFormats = DefaultSwaggerFormats

  def apiPath = "swagger.json"
  def apiVersion: String = "1.0.0"

  private val swagger = {
    val swagger = new m.Swagger
    val info = new m.Info()
      .version(apiVersion)
      .title("Swagger Petstore")

    val contact = new m.Contact()
      .name("Wordnik API Team")
      .email("foo@bar.baz")
      .url("http://swagger.io")
    info.setContact(contact)

    swagger.setInfo(info)
    swagger
  }

  GET / apiPath |>> { () =>
    val swaggerJson = Json.mapper().writeValueAsString(swagger)
    Ok(swaggerJson).withHeaders(`Content-Type`(MediaType.`application/json`))
  }

  override protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    super.append(action)
    new ApiBuilder(apiVersion, swagger).actionToApiListing(action)
  }
}
