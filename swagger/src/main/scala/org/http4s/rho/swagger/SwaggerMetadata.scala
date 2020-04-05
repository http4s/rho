package org.http4s.rho.swagger

import org.http4s.rho.swagger.models._

case class SwaggerMetadata(
           apiInfo: Info = Info(title = "My API", version = "1.0.0"),
           host: Option[String] = None,
           basePath: Option[String] = None,
           schemes: List[Scheme] = Nil,
           consumes: List[String] = Nil,
           produces: List[String] = Nil,
           security: List[SecurityRequirement] = Nil,
           securityDefinitions: Map[String, SecuritySchemeDefinition] = Map.empty,
           tags: List[Tag] = Nil,
           vendorExtensions: Map[String, AnyRef] = Map.empty){

  def toSwagger(): Swagger = Swagger(
    info = Some(apiInfo),
    host = host,
    basePath = basePath,
    schemes = schemes,
    consumes = consumes,
    produces = produces,
    security = security,
    securityDefinitions = securityDefinitions,
    tags = tags,
    vendorExtensions = vendorExtensions
  )
}
