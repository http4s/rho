package org.http4s
package rho
package swagger

import org.specs2.mutable.Specification
import cats.data.NonEmptyList
import cats.syntax.all._
import org.http4s.rho.bits.MethodAliases.GET

class SwaggerSupportSpec extends Specification {

  import org.json4s.JsonAST._
  import org.json4s.jackson._


  val baseService = new RhoService {
    GET / "hello" |>> { () => Ok("hello world") }
    GET / "hello"/ pathVar[String] |>> { world: String => Ok("hello " + world) }
  }

  val moarRoutes = new RhoService {
    GET / "goodbye" |>> { () => Ok("goodbye world") }
    GET / "goodbye"/ pathVar[String] |>> { world: String => Ok("goodbye " + world) }
  }


  "SwaggerSupport" should {
    "Expose an API listing" in {
      val service = baseService.toService(SwaggerSupport(swaggerRoutesInSwagger = true))

      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/swagger.json", "/hello", "/hello/{string}")
    }

    "Support prefixed routes" in {
      val service = ("foo" /: baseService).toService(SwaggerSupport(swaggerRoutesInSwagger = true))
      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/swagger.json", "/foo/hello", "/foo/hello/{string}")
    }

    "Provide a method to build the Swagger model for a list of routes" in {
      val swaggerSpec = SwaggerSupport.createSwagger(apiPath = "/api")(baseService.getRoutes)

      swaggerSpec.paths must haveSize(2)
    }

    "Provide a way to agregate routes from multiple RhoServices" in {
      val aggregateSwagger = SwaggerSupport.createSwagger()(baseService.getRoutes ++ moarRoutes.getRoutes)
      val swaggerRoutes = SwaggerSupport.createSwaggerRoute(aggregateSwagger)
      val httpServices = NonEmptyList.of(baseService, moarRoutes, swaggerRoutes).map(_.toService())
      val allthogetherService = httpServices.reduceLeft(Service.withFallback(_)(_))

      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)), (d, JObject(_)))) =
        parseJson(RRunner(allthogetherService).checkOk(r)) \\ "paths"

      Set(a, b, c, d) should_== Set("/hello", "/hello/{string}", "/goodbye", "/goodbye/{string}")
    }

  }
}
