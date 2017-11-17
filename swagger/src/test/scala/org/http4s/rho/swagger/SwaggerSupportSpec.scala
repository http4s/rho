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

  val trailingSlashService = new RhoService {
    GET / "foo" / "" |>> { () => Ok("hello world") }
  }

  val mixedTrailingSlashesService = new RhoService {
    GET / "foo" / "" |>> { () => Ok("hello world") }
    GET / "foo" |>> { () => Ok("hello world") }
    GET / "bar" |>> { () => Ok("hello world") }
  }

  val metaDataService = new RhoService {
    "Hello" ** GET / "hello" |>> { () => Ok("hello world") }
    Map("hello"->List("bye")) ^^ "Bye" ** GET / "bye" |>> { () => Ok("bye world") }
    Map("bye"->List("hello")) ^^ GET / "goodbye" |>> { () => Ok("goodbye world") }
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

    "Support endpoints which end in a slash" in {
      val service = trailingSlashService.toService(SwaggerSupport())
      val r = Request(GET, Uri(path = "/swagger.json"))
      val JObject(List((a, JObject(_)))) = parseJson(RRunner(service).checkOk(r)) \\ "paths"

      a should_== "/foo/"
    }

    "Support endpoints which end in a slash being mixed with normal endpoints"  in {
      val service = mixedTrailingSlashesService.toService(SwaggerSupport())
      val r = Request(GET, Uri(path = "/swagger.json"))
      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) = parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/foo/", "/foo", "/bar")
    }

    "Provide a way to agregate routes from multiple RhoServices, with mixed trailing slashes and non-trailing slashes" in {
      val aggregateSwagger = SwaggerSupport.createSwagger()(baseService.getRoutes ++ moarRoutes.getRoutes  ++ mixedTrailingSlashesService.getRoutes)
      val swaggerRoutes = SwaggerSupport.createSwaggerRoute(aggregateSwagger)
      val httpServices = NonEmptyList.of(baseService, moarRoutes, swaggerRoutes).map(_.toService())
      val allthogetherService = httpServices.reduceLeft(Service.withFallback(_)(_))

      val r = Request(GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)), (d, JObject(_)), (e, JObject(_)), (f, JObject(_)), (g, JObject(_)))) =
        parseJson(RRunner(allthogetherService).checkOk(r)) \\ "paths"

      Set(a, b, c, d, e, f, g) should_== Set("/hello", "/hello/{string}", "/goodbye", "/goodbye/{string}", "/foo/", "/foo", "/bar")
    }

    "Check metadata in API listing" in {
      val service = metaDataService.toService(SwaggerSupport(swaggerRoutesInSwagger = true))

      val r = Request(GET, Uri(path = "/swagger.json"))

      val json = parseJson(RRunner(service).checkOk(r))

      val JObject(List((a, JArray(List(JObject(List((e,JArray(List(JString(f))))))))), (b, JArray(List(JObject(List((g,JArray(List(JString(h))))))))))) = json \\ "security"
      val JObject(List((c, JString(i)), (d, JString(j)), _)) = json \\ "summary"

      Set(a,b) should_== Set("security", "security")
      Set(c,d) should_== Set("summary", "summary")
      Set(e,f) should_== Set("hello", "bye")
      Set(g,h) should_== Set("bye", "hello")
      Set(i,j) should_== Set("Hello", "Bye")
    }
  }
}
