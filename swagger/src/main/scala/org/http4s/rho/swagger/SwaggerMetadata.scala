/*
 * Copyright 2014 http4s.org
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.http4s.rho.swagger

import org.http4s.rho.swagger.models._

final case class SwaggerMetadata(
    apiInfo: Info = Info(title = "My API", version = "1.0.0"),
    host: Option[String] = None,
    basePath: Option[String] = None,
    schemes: List[Scheme] = Nil,
    consumes: List[String] = Nil,
    produces: List[String] = Nil,
    security: List[SecurityRequirement] = Nil,
    securityDefinitions: Map[String, SecuritySchemeDefinition] = Map.empty,
    tags: List[Tag] = Nil,
    vendorExtensions: Map[String, AnyRef] = Map.empty) {

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
