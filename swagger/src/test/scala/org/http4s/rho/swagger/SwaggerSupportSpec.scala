package org.http4s
package rho
package swagger

import cats.data.NonEmptyList
import cats.effect.IO
import org.http4s.rho.bits.MethodAliases.GET
import org.http4s.rho.io._
import org.http4s.rho.swagger.models._
import org.http4s.rho.swagger.syntax.io._
import org.specs2.mutable.Specification

class SwaggerSupportSpec extends Specification {
  import org.json4s.JsonAST._
  import org.json4s.jackson._

  val baseRoutes = new RhoRoutes[IO] {
    GET / "hello" |>> { () => Ok("hello world") }
    GET / "hello" / pathVar[String] |>> { world: String => Ok("hello " + world) }
  }

  val moarRoutes = new RhoRoutes[IO] {
    GET / "goodbye" |>> { () => Ok("goodbye world") }
    GET / "goodbye" / pathVar[String] |>> { world: String => Ok("goodbye " + world) }
  }

  val trailingSlashRoutes = new RhoRoutes[IO] {
    GET / "foo" / "" |>> { () => Ok("hello world") }
  }

  val mixedTrailingSlashesRoutes = new RhoRoutes[IO] {
    GET / "foo" / "" |>> { () => Ok("hello world") }
    GET / "foo" |>> { () => Ok("hello world") }
    GET / "bar" |>> { () => Ok("hello world") }
  }

  val metaDataRoutes = new RhoRoutes[IO] {
    "Hello" ** GET / "hello" |>> { () => Ok("hello world") }
    Map("hello" -> List("bye")) ^^ "Bye" ** GET / "bye" |>> { () => Ok("bye world") }
    Map("bye" -> List("hello")) ^^ GET / "goodbye" |>> { () => Ok("goodbye world") }
  }

  "SwaggerSupport" should {
    "Expose an API listing" in {
      val service = baseRoutes.toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))

      val r = Request[IO](GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)), (d, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c, d) should_== Set("/swagger.json", "/swagger.yaml", "/hello", "/hello/{string}")
    }

    "Support prefixed routes" in {
      val service =
        ("foo" /: baseRoutes).toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))
      val r = Request[IO](GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)), (d, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c, d) should_== Set(
        "/swagger.json",
        "/swagger.yaml",
        "/foo/hello",
        "/foo/hello/{string}"
      )
    }

    "Provide a method to build the Swagger model for a list of routes" in {
      val swaggerSpec = createSwagger()(baseRoutes.getRoutes)

      swaggerSpec.paths must haveSize(2)
    }

    "Provide a way to aggregate routes from multiple RhoRoutes" in {
      val aggregateSwagger = createSwagger()(baseRoutes.getRoutes ++ moarRoutes.getRoutes)
      val swaggerRoutes = createSwaggerRoute(aggregateSwagger)
      val httpRoutes = NonEmptyList.of(baseRoutes, moarRoutes, swaggerRoutes).reduceLeft(_ and _)

      val r = Request[IO](GET, Uri(path = "/swagger.json"))

      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)), (d, JObject(_)))) =
        parseJson(RRunner(httpRoutes.toRoutes()).checkOk(r)) \\ "paths"

      Set(a, b, c, d) should_== Set("/hello", "/hello/{string}", "/goodbye", "/goodbye/{string}")
    }

    "Support endpoints which end in a slash" in {
      val service = trailingSlashRoutes.toRoutes(createRhoMiddleware())
      val r = Request[IO](GET, Uri(path = "/swagger.json"))
      val JObject(List((a, JObject(_)))) = parseJson(RRunner(service).checkOk(r)) \\ "paths"

      a should_== "/foo/"
    }

    "Support endpoints which end in a slash being mixed with normal endpoints" in {
      val service = mixedTrailingSlashesRoutes.toRoutes(createRhoMiddleware())
      val r = Request[IO](GET, Uri(path = "/swagger.json"))
      val JObject(List((a, JObject(_)), (b, JObject(_)), (c, JObject(_)))) =
        parseJson(RRunner(service).checkOk(r)) \\ "paths"

      Set(a, b, c) should_== Set("/foo/", "/foo", "/bar")
    }

    "Provide a way to agregate routes from multiple RhoRoutes, with mixed trailing slashes and non-trailing slashes" in {
      val aggregateSwagger = createSwagger()(
        baseRoutes.getRoutes ++ moarRoutes.getRoutes ++ mixedTrailingSlashesRoutes.getRoutes
      )
      val swaggerRoutes = createSwaggerRoute(aggregateSwagger)
      val httpRoutes = NonEmptyList.of(baseRoutes, moarRoutes, swaggerRoutes).reduceLeft(_ and _)

      val r = Request[IO](GET, Uri(path = "/swagger.json"))

      val JObject(
        List(
          (a, JObject(_)),
          (b, JObject(_)),
          (c, JObject(_)),
          (d, JObject(_)),
          (e, JObject(_)),
          (f, JObject(_)),
          (g, JObject(_))
        )
      ) =
        parseJson(RRunner(httpRoutes.toRoutes()).checkOk(r)) \\ "paths"

      Set(a, b, c, d, e, f, g) should_== Set(
        "/hello",
        "/hello/{string}",
        "/goodbye",
        "/goodbye/{string}",
        "/foo/",
        "/foo",
        "/bar"
      )
    }

    "Check metadata in API listing" in {
      val service = metaDataRoutes.toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))

      val r = Request[IO](GET, Uri(path = "/swagger.json"))

      val json = parseJson(RRunner(service).checkOk(r))

      val JObject(
        List(
          (a, JArray(List(JObject(List((e, JArray(List(JString(f))))))))),
          (b, JArray(List(JObject(List((g, JArray(List(JString(h)))))))))
        )
      ) = json \\ "security"
      val JObject(List(_, (c, JString(i)), _, (d, JString(j)))) = json \\ "summary"

      Set(a, b) should_== Set("security", "security")
      Set(c, d) should_== Set("summary", "summary")
      Set(e, f) should_== Set("hello", "bye")
      Set(g, h) should_== Set("bye", "hello")
      Set(i, j) should_== Set("Hello", "Bye")
    }

    "Swagger support for complex meta data" in {
      val service = baseRoutes.toRoutes(
        createRhoMiddleware(
          jsonApiPath = "swagger-test.json",
          yamlApiPath = "swagger-test.yaml",
          swaggerRoutesInSwagger = true,
          swaggerMetadata = SwaggerMetadata(
            apiInfo = Info(
              title = "Complex Meta Data API",
              description = Some("Complex Meta Data API to verify in unit test"),
              version = "1.0.0",
              contact =
                Some(Contact("Name", Some("http://www.test.com/contact"), Some("test@test.com"))),
              license =
                Some(License("Apache 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html")),
              termsOfService = Some("http://www.test.com/tos")
            ),
            host = Some("www.test.com"),
            schemes = List(Scheme.HTTP, Scheme.HTTPS),
            basePath = Some("/v1"),
            consumes = List("application/json"),
            produces = List("application/json"),
            security = List(
              SecurityRequirement("apiKey", Nil),
              SecurityRequirement("vendor_jwk", List("admin"))
            ),
            securityDefinitions = Map(
              "api_key" -> ApiKeyAuthDefinition("key", In.QUERY, None),
              "vendor_jwk" -> OAuth2VendorExtensionsDefinition(
                authorizationUrl = "https://www.test.com/authorize",
                flow = "implicit",
                vendorExtensions = Map(
                  "x-vendor-issuer" -> "https://www.test.com/",
                  "x-vendor-jwks_uri" -> "https://www.test.com/.well-known/jwks.json",
                  "x-vendor-audiences" -> "clientid"
                ),
                scopes = Map("openid" -> "Open ID info", "admin" -> "Admin rights")
              )
            ),
            vendorExtensions = Map(
              "x-vendor-endpoints" -> Map("name" -> "www.test.com", "target" -> "298.0.0.1")
            )
          )
        )
      )

      val r = Request[IO](GET, Uri(path = "/swagger-test.json"))
      val json = parseJson(RRunner(service).checkOk(r))

      val JString(icn) = json \ "info" \ "contact" \ "name"
      val JString(h) = json \ "host"
      val JArray(List(JString(s1), JString(s2))) = json \ "schemes"
      val JString(bp) = json \ "basePath"
      val JArray(List(JString(c))) = json \ "consumes"
      val JArray(List(JString(p))) = json \ "produces"
      val JArray(List(JArray(sec1))) = json \ "security" \ "apiKey"
      val JArray(List(JArray(List(JString(sec2))))) = json \ "security" \ "vendor_jwk"
      val JString(t) = json \ "securityDefinitions" \ "api_key" \ "type"
      val JString(vi) = json \ "securityDefinitions" \ "vendor_jwk" \ "x-vendor-issuer"
      val JString(ve) = json \ "x-vendor-endpoints" \ "target"

      Set(icn, h, s1, s2, bp, c, p, sec2, t, vi, ve) should_== Set(
        "Name",
        "www.test.com",
        "http",
        "https",
        "/v1",
        "application/json",
        "application/json",
        "admin",
        "apiKey",
        "https://www.test.com/",
        "298.0.0.1"
      )
      sec1 should_== Nil
    }

    "Check metadata in API listing" in {
      val service = metaDataRoutes.toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))

      val r = Request[IO](GET, Uri(path = "/swagger.json"))

      val json = parseJson(RRunner(service).checkOk(r))

      val JObject(
        List(
          (a, JArray(List(JObject(List((e, JArray(List(JString(f))))))))),
          (b, JArray(List(JObject(List((g, JArray(List(JString(h)))))))))
        )
      ) = json \\ "security"
      val JObject(List(_, (c, JString(i)), _, (d, JString(j)))) = json \\ "summary"

      Set(a, b) should_== Set("security", "security")
      Set(c, d) should_== Set("summary", "summary")
      Set(e, f) should_== Set("hello", "bye")
      Set(g, h) should_== Set("bye", "hello")
      Set(i, j) should_== Set("Hello", "Bye")
    }
  }
}
