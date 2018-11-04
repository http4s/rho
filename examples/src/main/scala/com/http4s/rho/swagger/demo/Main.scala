package com.http4s.rho.swagger.demo

import cats.data.Kleisli
import cats.implicits._
import cats.effect._
import org.http4s.implicits._
import org.http4s.rho.swagger.SwaggerMetadata
import org.http4s.rho.swagger.models.{Info, Tag}
import org.http4s.server.blaze.BlazeServerBuilder
import org.log4s.getLogger

import scala.concurrent.ExecutionContext.global
import org.http4s.HttpRoutes
import org.http4s.rho.swagger.SwaggerSupport
import com.http4s.rho.swagger.ui.SwaggerUiRoutes

object Main extends IOApp {
  private val logger = getLogger

  private val port: Int = Option(System.getenv("HTTP_PORT"))
    .map(_.toInt)
    .getOrElse(8080)

  logger.info(s"Starting Swagger example on '$port'")

  def run(args: List[String]): IO[ExitCode] =
    Blocker[IO].use { blocker =>
      
      val metadata = SwaggerMetadata(
          apiInfo = Info(title = "Rho demo", version = "1.2.3"),
          tags = List(Tag(name = "hello", description = Some("These are the hello routes.")))
      )

      type IOwithUser[A] = Kleisli[IO, SimpleUser, A]
      val middleware = SwaggerSupport[IOwithUser].createRhoMiddleware(swaggerMetadata = metadata)
 
      val myAuthedService: HttpRoutes[IO] = ExampleAuth.simpleAuthMiddlware {
        middleware(new MyAuthedRoutes[IO]().getRoutes).toAuthedService()
      }

      val swaggerUiRoutes = SwaggerUiRoutes[IO](swaggerUiPath="swagger-ui", swaggerSpecRelativePath="../swagger.json", blocker)
      val myRoutes = myAuthedService <+> swaggerUiRoutes.toRoutes()

      /*
      val swaggerUiRhoMiddleware = SwaggerUi[IO].createRhoMiddleware(blocker, swaggerMetadata = metadata)
      val myRoutes = new MyRoutes[IO](ioSwagger).toRoutes(swaggerUiRhoMiddleware)
      */

      BlazeServerBuilder[IO](global)
        .withHttpApp(myRoutes.orNotFound)
        .bindLocal(port)
        .serve.compile.drain
        .as(ExitCode.Success)
    }
}
