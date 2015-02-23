package org.http4s.rho.swagger

import com.wordnik.swagger.{models => jm}

import scala.collection.JavaConversions._

object models {

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
      s.host(host.getOrElse(null))
      s.basePath(basePath.getOrElse(null))
      s.setSchemes(schemes.map(_.toJModel))
      s.setConsumes(consumes)
      s.setProduces(produces)
      s.setPaths(paths.mapValues(_.toJModel))
      s.setSecurityDefinitions(securityDefinitions.mapValues(_.toJModel))
      s.setDefinitions(definitions.mapValues(_.toJModel))
      s.setParameters(parameters.mapValues(_.toJModel))
      s.setExternalDocs(externalDocs.map(_.toJModel).getOrElse(null))
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
        .description(description.getOrElse(null))
        .termsOfService(termsOfService.getOrElse(null))
        .contact(contact.map(_.toJModel).getOrElse(null))
        .license(license.map(_.toJModel).getOrElse(null))
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
      (new jm.Contact).name(name).url(url.getOrElse(null)).email(email.getOrElse(null))
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
      oa2d.setScopes(scopes)
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
      sr.setScopes(scopes)
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
      sd.setScopes(scopes)
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
      p.setGet(get.map(_.toJModel).getOrElse(null))
      p.setPut(put.map(_.toJModel).getOrElse(null))
      p.setPost(post.map(_.toJModel).getOrElse(null))
      p.setDelete(delete.map(_.toJModel).getOrElse(null))
      p.setPatch(patch.map(_.toJModel).getOrElse(null))
      p.setOptions(options.map(_.toJModel).getOrElse(null))
      p.setParameters(parameters.map(_.toJModel))
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
      o.tags(tags)
      o.summary(summary.getOrElse(null))
      o.description(description.getOrElse(null))
      o.operationId(operationId.getOrElse(null))
      o.schemes(schemes.map(_.toJModel))
      o.consumes(consumes)
      o.produces(produces)
      o.setParameters(parameters.map(_.toJModel))
      o.setResponses(responses.mapValues(_.toJModel))
      o.setSecurity(security.map { m =>
        m.mapValues(xs => xs : java.util.List[String]) : java.util.Map[String, java.util.List[String]]
      })
      o.setExternalDocs(externalDocs.map(_.toJModel).getOrElse(null))
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
      r.setSchema(schema.map(_.toJModel).getOrElse(null))
      r.setExamples(examples)
      r.setHeaders(headers.mapValues(_.toJModel))
      r
    }
  }

  sealed trait Model {
    def description: String
    def properties: Map[String, Property]
    def example: Option[String]
    def externalDocs : Option[ExternalDocs]

    def toJModel: jm.Model
  }

  case class ModelImpl
    (
      description          : String
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
      m.setType(`type`.getOrElse(null))
      m.setName(name.getOrElse(null))
      m.setDescription(description)
      m.setRequired(required)
      m.setExample(example.getOrElse(null))
      m.setProperties(properties.mapValues(_.toJModel))
      m.setAdditionalProperties(additionalProperties.map(_.toJModel).getOrElse(null))
      m.setDiscriminator(discriminator.getOrElse(null))
      m.setExternalDocs(externalDocs.map(_.toJModel).getOrElse(null))
      m
    }
  }

  case class ArrayModel
    (
      description  : String
    ,`type`        : Option[String]        = None
    , properties   : Map[String, Property] = Map.empty
    , items        : Option[Property]      = None
    , example      : Option[String]        = None
    , externalDocs : Option[ExternalDocs]  = None
    ) extends Model {

    def toJModel: jm.Model = {
      val am = new jm.ArrayModel
      am.setType(`type`.getOrElse(null))
      am.setDescription(description)
      am.setProperties(properties.mapValues(_.toJModel))
      am.setItems(items.map(_.toJModel).getOrElse(null))
      am.setExample(example.getOrElse(null))
      am.setExternalDocs(externalDocs.map(_.toJModel).getOrElse(null))
      am
    }
  }

  case class ComposedModel
    (
      description  : String
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
      cm.setDescription(description)
      cm.setAllOf(allOf.map(_.toJModel))
      cm.setParent(parent.map(_.toJModel).getOrElse(null))
      cm.setChild(child.map(_.toJModel).getOrElse(null))
      cm.setInterfaces(interfaces.map(_.toJModel.asInstanceOf[jm.RefModel]))
      cm.setProperties(properties.mapValues(_.toJModel))
      cm.setExample(example.getOrElse(null))
      cm.setExternalDocs(externalDocs.map(_.toJModel).getOrElse(null))
      cm
    }
  }

  case class RefModel
    (
      ref          : String
    , description  : String
    , properties   : Map[String, Property] = Map.empty
    , example      : Option[String]        = None
    , externalDocs : Option[ExternalDocs]  = None
    ) extends Model {

    def toJModel: jm.Model = {
      val rm = new jm.RefModel(ref)
      rm.setDescription(description)
      rm.setProperties(properties.mapValues(_.toJModel))
      rm.setExample(example.getOrElse(null))
      rm.setExternalDocs(externalDocs.map(_.toJModel).getOrElse(null))
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
      bp.setSchema(schema.map(_.toJModel).getOrElse(null))
      bp.setName(name.getOrElse(null))
      bp.setDescription(description.getOrElse(null))
      bp.setRequired(required)
      bp.setAccess(access.getOrElse(null))
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
      cp.setFormat(format.getOrElse(null))
      cp.setCollectionFormat(collectionFormat.getOrElse(null))
      cp.setItems(items.map(_.toJModel).getOrElse(null))
      cp.setName(name.getOrElse(null))
      cp.setDescription(description.getOrElse(null))
      cp.setRequired(required)
      cp.setAccess(access.getOrElse(null))      
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
      fp.setFormat(format.getOrElse(null))
      fp.setCollectionFormat(collectionFormat.getOrElse(null))
      fp.setItems(items.map(_.toJModel).getOrElse(null))
      fp.setDefaultValue(defaultValue.getOrElse(null))
      fp.setName(name.getOrElse(null))
      fp.setDescription(description.getOrElse(null))
      fp.setRequired(required)
      fp.setAccess(access.getOrElse(null))
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
      hp.setFormat(format.getOrElse(null))
      hp.setCollectionFormat(collectionFormat.getOrElse(null))
      hp.setItems(items.map(_.toJModel).getOrElse(null))
      hp.setDefaultValue(defaultValue.getOrElse(null))
      hp.setName(name.getOrElse(null))
      hp.setDescription(description.getOrElse(null))
      hp.setRequired(required)
      hp.setAccess(access.getOrElse(null))
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
      pp.setFormat(format.getOrElse(null))
      pp.setCollectionFormat(collectionFormat.getOrElse(null))
      pp.setItems(items.map(_.toJModel).getOrElse(null))
      pp.setDefaultValue(defaultValue.getOrElse(null))
      pp.setName(name.getOrElse(null))
      pp.setDescription(description.getOrElse(null))
      pp.setRequired(required)
      pp.setAccess(access.getOrElse(null))
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
      qp.setFormat(format.getOrElse(null))
      qp.setCollectionFormat(collectionFormat.getOrElse(null))
      qp.setItems(items.map(_.toJModel).getOrElse(null))
      qp.setDefaultValue(defaultValue.getOrElse(null))
      qp.setName(name.getOrElse(null))
      qp.setDescription(description.getOrElse(null))
      qp.setRequired(required)
      qp.setAccess(access.getOrElse(null))
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
      rp.setName(name.getOrElse(null))
      rp.setDescription(description.getOrElse(null))
      rp.setRequired(required)
      rp.setAccess(access.getOrElse(null))
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
      ap.setTitle(title.getOrElse(null))
      ap.setDescription(description.getOrElse(null))
      ap.setFormat(format.getOrElse(null))
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
      ap.setTitle(title.getOrElse(null))
      ap.setDescription(description.getOrElse(null))
      ap.setFormat(format.getOrElse(null))
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
      rp.setTitle(title.getOrElse(null))
      rp.setDescription(description.getOrElse(null))
      rp.setFormat(format.getOrElse(null))
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
}
