package org.http4s.rho.swagger

import com.wordnik.swagger.{models => jm}

import scala.collection.JavaConversions._

object models {
  import JValue._

  case class Swagger
    (
      swagger             : String                                = "2.0"
    , info                : Info
    , host                : Option[String]                        = None
    , basePath            : Option[String]                        = None
    , schemes             : List[Scheme]                          = Nil
    , consumes            : List[String]                          = Nil
    , produces            : List[String]                          = Nil
    , paths               : Map[String, Path]                     = Map.empty
    , securityDefinitions : Map[String, SecuritySchemeDefinition] = Map.empty
    , definitions         : Map[String, Model]                    = Map.empty
    , parameters          : Map[String, Parameter]                = Map.empty
    , externalDocs        : Option[ExternalDocs]                  = None
    ) {

    def toJModel: jm.Swagger = {
      val s = new jm.Swagger
      s.info(info.toJModel)
      s.host(fromOption(host))
      s.basePath(fromOption(basePath))
      s.setSchemes(fromList(schemes.map(_.toJModel)))
      s.setConsumes(fromList(consumes))
      s.setProduces(fromList(produces))
      s.setPaths(fromMap(paths.mapValues(_.toJModel)))
      s.setSecurityDefinitions(fromMap(securityDefinitions.mapValues(_.toJModel)))
      s.setDefinitions(fromMap(definitions.mapValues(_.toJModel)))
      s.setParameters(fromMap(parameters.mapValues(_.toJModel)))
      s.setExternalDocs(fromOption(externalDocs.map(_.toJModel)))
      s
    }
  }

  case class Info
    (
      title            : String
    , version          : String
    , description      : Option[String]   = None
    , termsOfService   : Option[String]   = None
    , contact          : Option[Contact]  = None
    , license          : Option[License]  = None
    , vendorExtensions : Map[String, Any] = Map.empty 
    ) {

    def toJModel: jm.Info = {
      val i = new jm.Info
      i.title(title)
        .version(version)
        .description(fromOption(description))
        .termsOfService(fromOption(termsOfService))
        .contact(fromOption(contact.map(_.toJModel)))
        .license(fromOption(license.map(_.toJModel)))
      vendorExtensions.foreach { case (key, value) => i.setVendorExtension(key, value) }
      i
    }
  }

  case class Contact
    (
      name  : String
    , url   : Option[String] = None
    , email : Option[String] = None
    ) {

    def toJModel: jm.Contact =
      (new jm.Contact).name(name).url(fromOption(url)).email(fromOption(email))
  }

  case class License
    (
      name  : String
    , url   : String
    ) {

    def toJModel: jm.License =
      (new jm.License).name(name).url(url)
  }

  sealed trait Scheme {
    def toJModel: jm.Scheme
  }
  object Scheme {
    object HTTP  extends Scheme { def toJModel: jm.Scheme = jm.Scheme.HTTP }
    object HTTPS extends Scheme { def toJModel: jm.Scheme = jm.Scheme.HTTPS }
    object WS    extends Scheme { def toJModel: jm.Scheme = jm.Scheme.WS }
    object WSS   extends Scheme { def toJModel: jm.Scheme = jm.Scheme.WSS }
  }

  sealed trait SecuritySchemeDefinition {
    def `type`: String

    def toJModel: jm.auth.SecuritySchemeDefinition      
  }

  case class OAuth2Definition
    (
      authorizationUrl : String
    , tokenUrl         : String
    , flow             : String
    , scopes           : Map[String, String]
    ) extends SecuritySchemeDefinition {

    override val `type` = "oauth2"

    def toJModel: jm.auth.OAuth2Definition = {
      val oa2d = new jm.auth.OAuth2Definition
      oa2d.setAuthorizationUrl(authorizationUrl)
      oa2d.setTokenUrl(tokenUrl)
      oa2d.setFlow(flow)
      oa2d.setScopes(fromMap(scopes))
      oa2d
    }
  }

  case class ApiKeyAuthDefinition 
    (
      name : String
    , in   : In
    ) extends SecuritySchemeDefinition {

    override val `type` = "apiKey"

    def toJModel: jm.auth.ApiKeyAuthDefinition = {
      val akad  = new jm.auth.ApiKeyAuthDefinition
      akad.name(name).in(in.toJModel)
    }
  }

  case object BasicAuthDefinition extends SecuritySchemeDefinition {
    override val `type` = "basic"

    def toJModel: jm.auth.BasicAuthDefinition = 
      new jm.auth.BasicAuthDefinition
  }

  sealed trait In {
    def toJModel: jm.auth.In
  }
  object In {
    case object HEADER extends In { def toJModel: jm.auth.In = jm.auth.In.HEADER }
    case object QUERY  extends In { def toJModel: jm.auth.In = jm.auth.In.QUERY  }
  }

  case class SecurityRequirement
    (
      name   : String
    , scopes : List[String]
    ) {

    def toJModel: jm.SecurityRequirement = {
      val sr = new jm.SecurityRequirement
      sr.setName(name)
      sr.setScopes(fromList(scopes))
      sr
    }
  }

  case class SecurityDefinition
    (
      `type` : String
    , scopes : Map[String, String]
    ) {

    def toJModel: jm.SecurityDefinition = {
      val sd = new jm.SecurityDefinition
      sd.setType(`type`)
      sd.setScopes(fromMap(scopes))
      sd
    }
  }

  case class SecurityScope
    (
      name        : String
    , description : String
    ) {

    def toJModel: jm.SecurityScope = 
      new jm.SecurityScope(name, description)
  }

  case class Path
    (
      get              : Option[Operation] = None
    , put              : Option[Operation] = None
    , post             : Option[Operation] = None
    , delete           : Option[Operation] = None
    , patch            : Option[Operation] = None
    , options          : Option[Operation] = None
    , parameters       : List[Parameter]   = Nil
    , vendorExtensions : Map[String, Any]  = Map.empty
    ) {

    def toJModel: jm.Path = {
      val p = new jm.Path
      p.setGet(fromOption(get.map(_.toJModel)))
      p.setPut(fromOption(put.map(_.toJModel)))
      p.setPost(fromOption(post.map(_.toJModel)))
      p.setDelete(fromOption(delete.map(_.toJModel)))
      p.setPatch(fromOption(patch.map(_.toJModel)))
      p.setOptions(fromOption(options.map(_.toJModel)))
      p.setParameters(fromList(parameters.map(_.toJModel)))
      vendorExtensions.foreach { case (key, value) => p.setVendorExtension(key, value) }
      p
    }
  }

  case class Operation
    (
      tags             : List[String]                    = Nil
    , summary          : Option[String]                  = None
    , description      : Option[String]                  = None
    , operationId      : Option[String]                  = None
    , schemes          : List[Scheme]                    = Nil
    , consumes         : List[String]                    = Nil
    , produces         : List[String]                    = Nil
    , parameters       : List[Parameter]                 = Nil
    , responses        : Map[String, Response]           = Map.empty
    , security         : List[Map[String, List[String]]] = Nil
    , externalDocs     : Option[ExternalDocs]            = None
    , deprecated       : Boolean                         = false
    , vendorExtensions : Map[String, Any]                = Map.empty
    ) {

    def toJModel: jm.Operation = {
      val o = new jm.Operation
      o.setTags(fromList(tags))
      o.setSummary(fromOption(summary))
      o.setDescription(fromOption(description))
      o.setOperationId(fromOption(operationId))
      o.setSchemes(fromList(schemes.map(_.toJModel)))
      o.setConsumes(fromList(consumes))
      o.setProduces(fromList(produces))
      o.setParameters(fromList(parameters.map(_.toJModel)))
      o.setResponses(fromMap(responses.mapValues(_.toJModel)))
      o.setSecurity(fromList(security.map { m =>
        m.mapValues(xs => xs : java.util.List[String]) : java.util.Map[String, java.util.List[String]]
      }))
      o.setExternalDocs(fromOption(externalDocs.map(_.toJModel)))
      o.setDeprecated(deprecated)
      vendorExtensions.foreach { case (key, value) => o.setVendorExtension(key, value) }
      o
    }      
  }

  case class Response
    (
      description : String
    , schema      : Option[Property]
    , examples    : Map[String, String]   = Map.empty
    , headers     : Map[String, Property] = Map.empty
    ) {

    def toJModel: jm.Response = {
      val r = new jm.Response
      r.setDescription(description)
      r.setSchema(fromOption(schema.map(_.toJModel)))
      r.setExamples(fromMap(examples))
      r.setHeaders(fromMap(headers.mapValues(_.toJModel)))
      r
    }
  }

  sealed trait Model {
    def id: String
    def description: Option[String]
    def properties: Map[String, Property]
    def example: Option[String]
    def externalDocs : Option[ExternalDocs]

    def toJModel: jm.Model
  }

  case class ModelImpl
    (
      id                   : String
    , description          : Option[String]        = None
    , `type`               : Option[String]        = None
    , name                 : Option[String]        = None
    , required             : List[String]          = Nil
    , properties           : Map[String, Property] = Map.empty
    , isSimple             : Boolean               = false
    , example              : Option[String]        = None
    , additionalProperties : Option[Property]      = None
    , discriminator        : Option[String]        = None
    , externalDocs         : Option[ExternalDocs]  = None
    ) extends Model {

    def toJModel: jm.Model = {
      val m = new jm.ModelImpl
      m.setType(fromOption(`type`))
      m.setName(fromOption(name))
      m.setDescription(fromOption(description))
      m.setRequired(required)
      m.setExample(fromOption(example))
      m.setProperties(fromMap(properties.mapValues(_.toJModel)))
      m.setAdditionalProperties(fromOption(additionalProperties.map(_.toJModel)))
      m.setDiscriminator(fromOption(discriminator))
      m.setExternalDocs(fromOption(externalDocs.map(_.toJModel)))
      m
    }
  }

  case class ArrayModel
    (
      id           : String
    , description  : Option[String]        = None
    ,`type`        : Option[String]        = None
    , properties   : Map[String, Property] = Map.empty
    , items        : Option[Property]      = None
    , example      : Option[String]        = None
    , externalDocs : Option[ExternalDocs]  = None
    ) extends Model {

    def toJModel: jm.Model = {
      val am = new jm.ArrayModel
      am.setType(fromOption(`type`))
      am.setDescription(fromOption(description))
      am.setProperties(fromMap(properties.mapValues(_.toJModel)))
      am.setItems(fromOption(items.map(_.toJModel)))
      am.setExample(fromOption(example))
      am.setExternalDocs(fromOption(externalDocs.map(_.toJModel)))
      am
    }
  }

  case class ComposedModel
    (
      id           : String
    , description  : Option[String]        = None
    , allOf        : List[Model]           = Nil
    , parent       : Option[Model]         = None
    , child        : Option[Model]         = None
    , interfaces   : List[RefModel]        = Nil
    , properties   : Map[String, Property] = Map.empty
    , example      : Option[String]        = None
    , externalDocs : Option[ExternalDocs]  = None
    ) extends Model {

    def toJModel: jm.Model = {
      val cm = new jm.ComposedModel
      cm.setDescription(fromOption(description))
      cm.setAllOf(fromList(allOf.map(_.toJModel)))
      cm.setParent(fromOption(parent.map(_.toJModel)))
      cm.setChild(fromOption(child.map(_.toJModel)))
      cm.setInterfaces(fromList(interfaces.map(_.toJModel.asInstanceOf[jm.RefModel])))
      cm.setProperties(fromMap(properties.mapValues(_.toJModel)))
      cm.setExample(fromOption(example))
      cm.setExternalDocs(fromOption(externalDocs.map(_.toJModel)))
      cm
    }
  }

  case class RefModel
    (
      id           : String
    , ref          : String
    , description  : Option[String]        = None
    , properties   : Map[String, Property] = Map.empty
    , example      : Option[String]        = None
    , externalDocs : Option[ExternalDocs]  = None
    ) extends Model {

    def toJModel: jm.Model = {
      val rm = new jm.RefModel(ref)
      rm.setDescription(fromOption(description))
      rm.setProperties(fromMap(properties.mapValues(_.toJModel)))
      rm.setExample(fromOption(example))
      rm.setExternalDocs(fromOption(externalDocs.map(_.toJModel)))
      rm
    }
  }

  sealed trait Parameter {
    def in: Option[String]
    def access: Option[String]
    def name: Option[String]
    def description: Option[String]
    def required: Boolean
    def vendorExtensions: Map[String, Any]

    def toJModel: jm.parameters.Parameter
  }

  case class BodyParameter
    (
      schema           : Option[Model]    = None
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = Some("body")

    def toJModel: jm.parameters.Parameter = {
      val bp = new jm.parameters.BodyParameter
      bp.setSchema(fromOption(schema.map(_.toJModel)))
      bp.setName(fromOption(name))
      bp.setDescription(fromOption(description))
      bp.setRequired(required)
      bp.setAccess(fromOption(access))
      vendorExtensions.foreach { case (key, value) => bp.setVendorExtension(key, value) }
      bp
    }
  }

  case class CookieParameter
    (
      `type`           : String
    , format           : Option[String]   = None
    , collectionFormat : Option[String]   = None
    , items            : Option[Property] = None
    , defaultValue     : Option[String]   = None
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = Some("cookie")

    def toJModel: jm.parameters.Parameter = {
      val cp = new jm.parameters.CookieParameter
      cp.setType(`type`)
      cp.setFormat(fromOption(format))
      cp.setCollectionFormat(fromOption(collectionFormat))
      cp.setItems(fromOption(items.map(_.toJModel)))
      cp.setName(fromOption(name))
      cp.setDescription(fromOption(description))
      cp.setRequired(required)
      cp.setAccess(fromOption(access))      
      vendorExtensions.foreach { case (key, value) => cp.setVendorExtension(key, value) }
      cp
    }
  }

  case class FormParameter
    (
      `type`           : String
    , format           : Option[String]   = None
    , collectionFormat : Option[String]   = None
    , items            : Option[Property] = None
    , defaultValue     : Option[String]   = None
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = Some("formData")

    def toJModel: jm.parameters.Parameter = {
      val fp = new jm.parameters.FormParameter
      fp.setType(`type`)
      fp.setFormat(fromOption(format))
      fp.setCollectionFormat(fromOption(collectionFormat))
      fp.setItems(fromOption(items.map(_.toJModel)))
      fp.setDefaultValue(fromOption(defaultValue))
      fp.setName(fromOption(name))
      fp.setDescription(fromOption(description))
      fp.setRequired(required)
      fp.setAccess(fromOption(access))
      vendorExtensions.foreach { case (key, value) => fp.setVendorExtension(key, value) }
      fp
    }    
  }

  case class HeaderParameter
    (
      `type`           : String
    , format           : Option[String]   = None
    , collectionFormat : Option[String]   = None
    , items            : Option[Property] = None
    , defaultValue     : Option[String]   = None
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = Some("header")

    def toJModel: jm.parameters.Parameter = {
      val hp = new jm.parameters.HeaderParameter
      hp.setType(`type`)
      hp.setFormat(fromOption(format))
      hp.setCollectionFormat(fromOption(collectionFormat))
      hp.setItems(fromOption(items.map(_.toJModel)))
      hp.setDefaultValue(fromOption(defaultValue))
      hp.setName(fromOption(name))
      hp.setDescription(fromOption(description))
      hp.setRequired(required)
      hp.setAccess(fromOption(access))
      vendorExtensions.foreach { case (key, value) => hp.setVendorExtension(key, value) }
      hp
    }
  }

  case class PathParameter
    (
      `type`           : String
    , format           : Option[String]   = None
    , collectionFormat : Option[String]   = None
    , items            : Option[Property] = None
    , defaultValue     : Option[String]   = None
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = Some("path")

    def toJModel: jm.parameters.Parameter = {
      val pp = new jm.parameters.PathParameter
      pp.setType(`type`)
      pp.setFormat(fromOption(format))
      pp.setCollectionFormat(fromOption(collectionFormat))
      pp.setItems(fromOption(items.map(_.toJModel)))
      pp.setDefaultValue(fromOption(defaultValue))
      pp.setName(fromOption(name))
      pp.setDescription(fromOption(description))
      pp.setRequired(required)
      pp.setAccess(fromOption(access))
      vendorExtensions.foreach { case (key, value) => pp.setVendorExtension(key, value) }
      pp
    }
      
  }

  case class QueryParameter
    (
      `type`           : String
    , format           : Option[String]   = None
    , collectionFormat : Option[String]   = None
    , items            : Option[Property] = None
    , defaultValue     : Option[String]   = None
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = Some("query")

    def toJModel: jm.parameters.Parameter = {
      val qp = new jm.parameters.QueryParameter
      qp.setType(`type`)
      qp.setFormat(fromOption(format))
      qp.setCollectionFormat(fromOption(collectionFormat))
      qp.setItems(fromOption(items.map(_.toJModel)))
      qp.setDefaultValue(fromOption(defaultValue))
      qp.setName(fromOption(name))
      qp.setDescription(fromOption(description))
      qp.setRequired(required)
      qp.setAccess(fromOption(access))
      vendorExtensions.foreach { case (key, value) => qp.setVendorExtension(key, value) }
      qp
    }      
  }

  case class RefParameter
    (
      ref              : String
    , name             : Option[String]   = None
    , description      : Option[String]   = None
    , required         : Boolean          = false
    , access           : Option[String]   = None
    , vendorExtensions : Map[String, Any] = Map.empty
    ) extends Parameter {

    override val in = None

    def toJModel: jm.parameters.Parameter = {
      val rp = new jm.parameters.RefParameter(ref)
      rp.setName(fromOption(name))
      rp.setDescription(fromOption(description))
      rp.setRequired(required)
      rp.setAccess(fromOption(access))
      vendorExtensions.foreach { case (key, value) => rp.setVendorExtension(key, value) }
      rp
    }
  }

  sealed trait Property {
    def `type`: String
    def required: Boolean
    def title: Option[String]
    def description: Option[String]
    def format: Option[String]

    def withRequired(required: Boolean): Property
    def toJModel: jm.properties.Property
  }

  case class AbstractProperty
    (
      `type`      : String
    , required    : Boolean        = false
    , title       : Option[String] = None
    , description : Option[String] = None
    , format      : Option[String] = None
    ) extends Property {

    def withRequired(required: Boolean): AbstractProperty =
      copy(required = required)

    def toJModel: jm.properties.Property = {
      val ap = new jm.properties.AbstractProperty {}
      ap.setType(`type`)
      ap.setRequired(required)
      ap.setTitle(fromOption(title))
      ap.setDescription(fromOption(description))
      ap.setFormat(fromOption(format))
      ap
    }
  }

  case class ArrayProperty
    (
      items       : Property
    , uniqueItems : Boolean
    , required    : Boolean        = false
    , title       : Option[String] = None
    , description : Option[String] = None
    , format      : Option[String] = None
    ) extends Property {

    override val `type` = "array"

    def withRequired(required: Boolean): ArrayProperty =
      this.copy(required = required)

    def toJModel: jm.properties.Property = {
      val ap = new jm.properties.ArrayProperty
      ap.setItems(items.toJModel)
      ap.setUniqueItems(uniqueItems)
      ap.setTitle(fromOption(title))
      ap.setDescription(fromOption(description))
      ap.setFormat(fromOption(format))
      ap
    }
  }

  case class RefProperty
    (
      ref         : String
    , required    : Boolean        = false
    , title       : Option[String] = None
    , description : Option[String] = None
    , format      : Option[String] = None
    ) extends Property {

    override val `type` = "ref"

    def withRequired(required: Boolean): RefProperty =
      copy(required = required)

    def toJModel: jm.properties.Property = {
      val rp = new jm.properties.RefProperty(ref)
      rp.setRequired(required)
      rp.setTitle(fromOption(title))
      rp.setDescription(fromOption(description))
      rp.setFormat(fromOption(format))
      rp
    }
  }

  case class ExternalDocs
    (
      description : String
    , url         : String
    ) {

    def toJModel: jm.ExternalDocs = {
      val ed = new jm.ExternalDocs
      ed.setDescription(description)
      ed.setUrl(url)
      ed
    }      
  }

  private object JValue {

    def fromOption[A](oa: Option[A]): A =
      oa.getOrElse(null.asInstanceOf[A])

    def fromList[A](xs: List[A]): java.util.List[A] =
      if (xs.isEmpty) null else xs

    def fromMap[A, B](m: Map[A, B]): java.util.Map[A, B] =
      if (m.isEmpty) null else m
  }
}
