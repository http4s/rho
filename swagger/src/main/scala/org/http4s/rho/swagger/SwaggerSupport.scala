package org.http4s
package rho
package swagger

import Header.`Content-Type`
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.model.{ApiListing, ApiInfo, SwaggerSerializers}

import shapeless.HList

import org.json4s._
import org.json4s.jackson.JsonMethods._
import JsonDSL._

import scodec.bits.ByteVector

trait SwaggerSupport extends RhoService with ApiBuilder {

  def apiVersion: String = "1.0"
  def apiInfo: ApiInfo = ApiInfo("None", "none", "none", "none", "none", "none")

  implicit protected def jsonFormats: Formats = SwaggerSerializers.formats

  private val swagger = new Swagger("1.2", apiVersion, apiInfo)

  GET / "api-info" |>> { () =>
    val json = swagger.resourceListing
    Status.Ok(compact(render(json)))
          .withHeaders(Header.`Content-Type`(MediaType.`application/json`))
  }

  GET / "api-info" / * |>> { params: Seq[String] =>
    swagger.getDoc(params) match {
      case Some(doc) =>
        val json = compact(render(doc))
        Status.Ok(compact(render(json)))
          .withHeaders(Header.`Content-Type`(MediaType.`application/json`))

      case None => Status.NotFound("api-info" + params.mkString("/", "/", ""))
    }
  }

  override protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    super.append(action)
    val apis = actionToApiListing(action)
    apis.foreach(_.apis.foreach(desc => ))//swagger.register(, _))
  }

  protected def docToJson(doc: Api): JValue = Extraction.decompose(doc)

  implicit def jsonWritable = new SimpleWritable[JValue] {
    override def contentType: `Content-Type` = `Content-Type`(MediaType.`application/json`)

    override def asChunk(data: _root_.org.json4s.JValue): ByteVector =
      ByteVector.view(compact(render(data)).getBytes(CharacterSet.`UTF-8`.charset))
  }
}
