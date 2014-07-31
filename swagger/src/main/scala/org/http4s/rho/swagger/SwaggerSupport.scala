package org.http4s
package rho
package swagger

import Header.`Content-Type`
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.model.{ApiInfo, SwaggerSerializers}
import org.http4s.Writable.Entity

import shapeless.HList

import org.json4s._
import org.json4s.jackson.JsonMethods._

import scodec.bits.ByteVector

import scalaz.concurrent.Task
import scalaz.stream.Process.emit

trait SwaggerSupport extends RhoService {
  implicit protected def jsonFormats: Formats = SwaggerSerializers.formats

  def apiPath = "api-info"
  def apiVersion: String = "1.0.0"
  def apiInfo: ApiInfo = ApiInfo("None", "none", "none", "none", "none", "none")

  private val swaggerBuilder = new ApiBuilder(apiVersion)
  private val swaggerStorage = new Swagger("1.2", apiVersion, apiInfo)

  GET / apiPath |>> { () =>
    val json = swaggerStorage.resourceListing
    Status.Ok(compact(render(json)))
      .withHeaders(Header.`Content-Type`(MediaType.`application/json`))
  }

  GET / apiPath / * |>> { params: Seq[String] =>
    swaggerStorage.getDoc(params) match {
      case Some(doc) =>
        Status.Ok(compact(render(doc)))
          .withHeaders(Header.`Content-Type`(MediaType.`application/json`))

      case None => Status.NotFound("Api Not Found: api-info" + params.mkString("/", "/", ""))
    }
  }

  override protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    super.append(action)
    val apis = swaggerBuilder.actionToApiListing(action)
    apis.foreach { listing => swaggerStorage.register(listing.resourcePath, listing) }
  }

  protected def docToJson(doc: Api): JValue = Extraction.decompose(doc)

  private implicit val jsonWritable: Writable[JValue] = {
    val headers: Headers = Headers(`Content-Type`(MediaType.`application/json`))
    Writable({jv: JValue =>
      val v = ByteVector.view(compact(render(jv)).getBytes(CharacterSet.`UTF-8`.charset))
      Task.now(Entity(emit(v), Some(v.length)))
    }, headers)
  }
}
