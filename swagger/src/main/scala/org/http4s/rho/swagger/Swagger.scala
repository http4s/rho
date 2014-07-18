package org.http4s.rho.swagger

import com.wordnik.swagger.model.{ApiListing, ApiInfo}

import org.json4s._
import org.json4s.JsonDSL._


class Swagger(swaggerVersion: String, apiVersion: String, apiInfo: ApiInfo) {
  import com.wordnik.swagger.model.SwaggerSerializers.formats

  private val apis = new scala.collection.concurrent.TrieMap[String, ApiListing]()

  def getApis: Iterable[ApiListing] = apis.values

  def resourceListing: JValue = renderIndex(apis.values.toSeq)

  def getDoc(path: Seq[String]): Option[JValue] = { // ApiListing
    apis.get(path.mkString("/", "/", "")).map(renderDoc)
  }

  def register(path: String, doc: ApiListing): Unit = apis.get(path) match {
    case Some(api) => apis += ((path, mergeApis(api, doc)))
    case None => apis += ((path, doc))
  }

  private def mergeApis(a: ApiListing, b: ApiListing): ApiListing = {
    println(s"WARNING: Not implemented. \n$a\n$b")

//    case class ApiListing (
//                            apiVersion: String,
//                            swaggerVersion: String,
//                            basePath: String,
//                            resourcePath: String,
//                            produces: List[String] = List.empty,
//                            consumes: List[String] = List.empty,
//                            protocols: List[String] = List.empty,
//                            authorizations: List[Authorization] = List.empty,
//                            apis: List[ApiDescription] = List(),--------
//    models: Option[Map[String, Model]] = None, |
//    description: Option[String] = None,        |
//    position: Int = 0)

    val models = (a.models, b.models) match {
      case (a@ Some(_), None) => a
      case (None, b@ Some(_)) => b
      case (Some(a), Some(b)) => Some(a ++ b)
      case _ => None
    }

    val description = (a.description, b.description) match {
      case (a@ Some(_), None) => a
      case (None, b@ Some(_)) => b
      case (Some(a), Some(b)) => Some(a + "; " + b)
      case _ => None
    }

    ApiListing(
      a.apiVersion,
      a.swaggerVersion,
      a.basePath,
      a.resourcePath,
      a.produces ++ b.produces,
      a.consumes ++ b.consumes,
      a.protocols ++ b.protocols,
      a.authorizations:::b.authorizations,
      a.apis:::b.apis,
      models,
      description,
      a.position
    )
  }

  private def renderDoc(doc: ApiListing): JValue = {
    val json = docToJson(doc) merge
      ("basePath" -> "http://localhost:8080") ~
        ("swaggerVersion" -> swaggerVersion) ~
        ("apiVersion" -> apiVersion)
    val consumes = dontAddOnEmpty("consumes", doc.consumes)_
    val produces = dontAddOnEmpty("produces", doc.produces)_
    val protocols = dontAddOnEmpty("protocols", doc.protocols)_
    val authorizations = dontAddOnEmpty("authorizations", doc.authorizations.map(_.`type`))_
    val jsonDoc = (consumes andThen produces andThen protocols andThen authorizations)(json)
    //    println("The rendered json doc:\n" + jackson.prettyJson(jsonDoc))
    jsonDoc
  }


  private def renderIndex(docs: Seq[ApiListing]): JValue = {
    ("apiVersion" -> apiVersion) ~
      ("swaggerVersion" -> swaggerVersion) ~
      ("apis" ->
        (docs.filter(_.apis.nonEmpty).map { doc =>
          ("path" -> doc.resourcePath) ~ ("description" -> doc.description)
        })) ~
      ("info" -> Option(apiInfo).map(Extraction.decompose(_)))
  }

  private def docToJson(doc: ApiListing): JValue = Extraction.decompose(doc)

  private[this] def dontAddOnEmpty(key: String, value: List[String])(json: JValue) = {
    val v: JValue = if (value.nonEmpty) key -> value else JNothing
    json merge v
  }
}
