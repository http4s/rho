package org.http4s
package rho
package swagger

import Header.`Content-Type`
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.model.{ApiInfo, SwaggerSerializers}

import shapeless.HList

import org.json4s._
import org.json4s.jackson.JsonMethods._

import scodec.bits.ByteVector

trait SwaggerSupport extends RhoService with ApiBuilder {

  def apiVersion: String = "1.0.0"
  def apiInfo: ApiInfo = ApiInfo("None", "none", "none", "none", "none", "none")

  implicit protected def jsonFormats: Formats = SwaggerSerializers.formats

  private val swagger = new Swagger("1.2", apiVersion, apiInfo)

  GET / "api-info" |>> { () =>
    val json = swagger.resourceListing
    Status.Ok(compact(render(json)))
      .withHeaders(Header.`Content-Type`(MediaType.`application/json`),
        Header.Raw(Header.`Access-Control-Allow-Origin`name, "*"))
  }

  GET / "api-info" / * |>> { params: Seq[String] =>

    println("----------------\nGetting " + params + "\n------------------" )
    swagger.getDoc(params) match {
      case Some(doc) =>
        Status.Ok(compact(render(doc)))
          .withHeaders(Header.`Content-Type`(MediaType.`application/json`),
                       Header.Raw(Header.`Access-Control-Allow-Origin`name, "*"))

      case None => Status.NotFound("Api Not Found: api-info" + params.mkString("/", "/", ""))
    }
  }

  override protected def append[T <: HList, F](action: RhoAction[T, F]): Unit = {
    super.append(action)
    val apis = actionToApiListing(action)
//    println("-------------------\n" + apis + "\n----------------------")
    apis.foreach { listing => swagger.register(listing.resourcePath, listing) }
  }

  protected def docToJson(doc: Api): JValue = Extraction.decompose(doc)

  implicit val jsonWritable = new SimpleWritable[JValue] {
    override def contentType: `Content-Type` = `Content-Type`(MediaType.`application/json`)

    override def asChunk(data: _root_.org.json4s.JValue): ByteVector =
      ByteVector.view(compact(render(data)).getBytes(CharacterSet.`UTF-8`.charset))
  }
}
