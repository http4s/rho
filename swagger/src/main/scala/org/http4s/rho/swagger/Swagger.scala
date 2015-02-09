//package org.http4s.rho.swagger
//
//import com.wordnik.swagger.models.Swagger
//
//import org.json4s._
//import org.json4s.JsonDSL._
//
//// TODO: It may be nice to make this cache the JSON docs as I bet they are a bit expensive to render
//class SwaggerRenderer(swaggerVersion: String, apiVersion: String, swagger: Swagger) {
//  import com.wordnik.swagger.model.SwaggerSerializers.formats
//
//  private var api = swagger
//
//  def getApi: Swagger = api
//
//  def resourceListing: JValue = renderIndex(apis.values.toSeq)
//
//  def register(path: String, doc: ApiListing): Unit = apis.get(path) match {
//    case Some(api) => apis += ((path, mergeApis(api, doc)))
//    case None => apis += ((path, doc))
//  }
//
//  private def mergeApis(a: Swagger, b: Swagger): Swagger = {
//
//    def mergeOption[A](a: Option[A], b: Option[A])(merge: (A, A) => A): Option[A] = (a, b) match {
//      case (a@ Some(_), None) => a
//      case (None, b@ Some(_)) => b
//      case (Some(a), Some(b)) => Some(merge(a, b))
//      case _ => None
//    }
//
//    val models = mergeOption(a.models, b.models)(_ ++ _)
//    val description = mergeOption(a.description, b.description)(_ + "; " + _)
//
//    ApiListing(
//      a.apiVersion,
//      a.swaggerVersion,
//      a.basePath,
//      a.resourcePath,
//      a.produces ::: b.produces,
//      a.consumes ::: b.consumes,
//      a.protocols ::: b.protocols,
//      a.authorizations ::: b.authorizations,
//      a.apis ::: b.apis,
//      models,
//      description,
//      a.position
//    )
//  }
//
//  private def renderDoc(doc: Swagger): JValue = {
//    val json = docToJson(doc) merge
//      ("basePath" -> "/") ~
//        ("swaggerVersion" -> swaggerVersion) ~
//        ("apiVersion" -> apiVersion)
//    val consumes = dontAddOnEmpty("consumes", doc.consumes)_
//    val produces = dontAddOnEmpty("produces", doc.produces)_
//    val protocols = dontAddOnEmpty("protocols", doc.protocols)_
//    val authorizations = dontAddOnEmpty("authorizations", doc.authorizations.map(_.`type`))_
//    val jsonDoc = (consumes andThen produces andThen protocols andThen authorizations)(json)
//    jsonDoc
//  }
//
//  private def docToJson(doc: ApiListing): JValue = Extraction.decompose(doc)
//
//  private[this] def dontAddOnEmpty(key: String, value: List[String])(json: JValue) = {
//    val v: JValue = if (value.nonEmpty) key -> value else JNothing
//    json merge v
//  }
//}
