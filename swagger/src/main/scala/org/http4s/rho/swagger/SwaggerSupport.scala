package org.http4s
package rho
package swagger

import java.nio.charset.StandardCharsets

import Header.`Content-Type`
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.model.{ApiInfo, SwaggerSerializers}
import org.http4s.Writable.Entity

import shapeless.HList

import org.json4s._
import org.json4s.jackson.JsonMethods._

import scodec.bits.ByteVector

import scalaz.{-\/, \/-}
import scalaz.concurrent.Task
import scalaz.stream.Process.emit

trait SwaggerSupport extends RhoService {
  implicit protected def jsonFormats: org.json4s.Formats = SwaggerSerializers.formats

  /** Override the `swaggerFormats` to add your own custom serializers */
  def swaggerFormats: SwaggerFormats = DefaultSwaggerFormats

  def apiPath = "api-info"
  def apiVersion: String = "1.0.0"
  def apiInfo: ApiInfo = ApiInfo("None", "none", "none", "none", "none", "none")

  private val swaggerStorage = new Swagger("1.2", apiVersion, apiInfo)

  GET / apiPath |>> { () =>
    val json = swaggerStorage.resourceListing
    Ok(compact(render(json)))
      .withHeaders(Header.`Content-Type`(MediaType.`application/json`))
  }

  GET / apiPath / * |>> { params: Seq[String] =>
    swaggerStorage.getDoc(params) match {
      case Some(doc) =>
        \/-(Ok(compact(render(doc)))
          .withHeaders(Header.`Content-Type`(MediaType.`application/json`)))

      case None =>
        -\/(NotFound("Api Not Found: api-info" + params.mkString("/", "/", "")))
    }
  }

  override protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    super.append(action)
    val apis = new ApiBuilder(apiVersion, swaggerFormats).actionToApiListing(action)
    apis.foreach { listing => swaggerStorage.register(listing.resourcePath, listing) }
  }

  protected def docToJson(doc: Api): JValue = Extraction.decompose(doc)

  private implicit val jsonWritable: Writable[JValue] = {
    val headers: Headers = Headers(`Content-Type`(MediaType.`application/json`))
    Writable({ jv: JValue =>
      val v = ByteVector.view(compact(render(jv)).getBytes(StandardCharsets.UTF_8))
      Task.now(Entity(emit(v), Some(v.length)))
    }, headers)
  }
}
