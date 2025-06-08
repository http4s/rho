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

package com.http4s.rho.swagger.demo

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import com.http4s.rho.swagger.ui.SwaggerUi
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.implicits._
import org.http4s.rho.swagger.SwaggerMetadata
import org.http4s.rho.swagger.models.Info
import org.http4s.rho.swagger.models.Tag
import org.http4s.rho.swagger.syntax.{io => ioSwagger}
import org.log4s.getLogger

object Main extends IOApp {
  private val logger = getLogger

  private val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] = {
    val metadata = SwaggerMetadata(
      apiInfo = Info(title = "Rho demo", version = "1.2.3"),
      tags = List(Tag(name = "hello", description = Some("These are the hello routes.")))
    )

    val swaggerUiRhoMiddleware =
      SwaggerUi[IO].createRhoMiddleware(swaggerMetadata = metadata)
    val myRoutes = new MyRoutes[IO](ioSwagger).toRoutes(swaggerUiRhoMiddleware)

    BlazeServerBuilder[IO]
      .withHttpApp(myRoutes.orNotFound)
      .bindLocal(port)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
  }
}
