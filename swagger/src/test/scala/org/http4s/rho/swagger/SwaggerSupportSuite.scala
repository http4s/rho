package org.http4s
package rho
package swagger

import cats.data._
import cats.effect.IO
import cats.syntax.applicative._
import _root_.io.circe.parser._
import _root_.io.circe._
import _root_.io.circe.syntax._
import munit.CatsEffectSuite
import org.http4s.rho.bits.MethodAliases.GET
import org.http4s.rho.io._
import org.http4s.rho.swagger.models._
import org.http4s.rho.swagger.syntax.io._

class SwaggerSupportSuite extends CatsEffectSuite {

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

  case class SwaggerRoot(paths: Map[String, Json] = Map.empty)

  implicit lazy val swaggerRootDecoder: Decoder[SwaggerRoot] =
    _root_.io.circe.generic.semiauto.deriveDecoder

  test("SwaggerSupport should expose an API listing") {
    val service = baseRoutes.toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))

    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))

    val swaggerRoot =
      RRunner(service)
        .checkOk(r)
        .map(decode[SwaggerRoot])
        .flatMap(
          _.map(_.paths.keySet.pure[IO])
            .getOrElse(IO.raiseError(new Throwable("Expected Right value")))
        )

    assertIO(
      swaggerRoot,
      Set(
        "/swagger.json",
        "/swagger.yaml",
        "/hello",
        "/hello/{string}"
      )
    )
  }

  test("SwaggerSupport should support prefixed routes") {
    val service =
      ("foo" /: baseRoutes).toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))
    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))

    val swaggerRoot =
      RRunner(service)
        .checkOk(r)
        .map(decode[SwaggerRoot])
        .flatMap(
          _.map(_.paths.keySet.pure[IO])
            .getOrElse(IO.raiseError(new Throwable("Expected Right value")))
        )

    assertIO(
      swaggerRoot,
      Set(
        "/swagger.json",
        "/swagger.yaml",
        "/foo/hello",
        "/foo/hello/{string}"
      )
    )
  }

  test("SwaggerSupport should provide a method to build the Swagger model for a list of routes") {
    val swaggerSpec = createSwagger()(baseRoutes.getRoutes)

    assertEquals(swaggerSpec.paths.size, 2)
  }

  test("SwaggerSupport should provide a way to aggregate routes from multiple RhoRoutes") {
    val aggregateSwagger = createSwagger()(baseRoutes.getRoutes ++ moarRoutes.getRoutes)
    val swaggerRoutes = createSwaggerRoute(aggregateSwagger)
    val httpRoutes = NonEmptyList.of(baseRoutes, moarRoutes, swaggerRoutes).reduceLeft(_ and _)

    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))

    val swaggerRoot = RRunner(httpRoutes.toRoutes())
      .checkOk(r)
      .map(decode[SwaggerRoot])
      .flatMap(
        _.map(_.paths.keySet.pure[IO])
          .getOrElse(IO.raiseError(new Throwable("Expected Right value")))
      )

    assertIO(
      swaggerRoot,
      Set(
        "/hello",
        "/hello/{string}",
        "/goodbye",
        "/goodbye/{string}"
      )
    )
  }

  test("SwaggerSupport should support endpoints which end in a slash") {
    val service = trailingSlashRoutes.toRoutes(createRhoMiddleware())
    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))

    val swaggerRoot = RRunner(service)
      .checkOk(r)
      .map(decode[SwaggerRoot])
      .flatMap(
        _.map(_.paths.keySet.pure[IO])
          .getOrElse(IO.raiseError(new Throwable("Expected Right value")))
      )

    assertIO(swaggerRoot.map(_.head), "/foo/")
  }

  test(
    "SwaggerSupport should support endpoints which end in a slash being mixed with normal endpoints"
  ) {
    val service = mixedTrailingSlashesRoutes.toRoutes(createRhoMiddleware())
    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))
    val swaggerRoot = RRunner(service)
      .checkOk(r)
      .map(decode[SwaggerRoot])
      .flatMap(
        _.map(_.paths.keySet.pure[IO])
          .getOrElse(IO.raiseError(new Throwable("Expected Right value")))
      )

    assertIO(swaggerRoot, Set("/foo/", "/foo", "/bar"))
  }

  test(
    "SwaggerSupport should provide a way to agregate routes from multiple RhoRoutes, with mixed trailing slashes and non-trailing slashes"
  ) {
    val aggregateSwagger = createSwagger()(
      baseRoutes.getRoutes ++ moarRoutes.getRoutes ++ mixedTrailingSlashesRoutes.getRoutes
    )
    val swaggerRoutes = createSwaggerRoute(aggregateSwagger)
    val httpRoutes = NonEmptyList.of(baseRoutes, moarRoutes, swaggerRoutes).reduceLeft(_ and _)

    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))

    val swaggerRoot = RRunner(httpRoutes.toRoutes())
      .checkOk(r)
      .map(decode[SwaggerRoot])
      .flatMap(
        _.map(_.paths.keySet.pure[IO])
          .getOrElse(IO.raiseError(new Throwable("Expected Right value")))
      )

    assertIO(
      swaggerRoot,
      Set(
        "/hello",
        "/hello/{string}",
        "/goodbye",
        "/goodbye/{string}",
        "/foo/",
        "/foo",
        "/bar"
      )
    )
  }

  test("SwaggerSupport should check metadata in API listing") {
    val service = metaDataRoutes.toRoutes(createRhoMiddleware(swaggerRoutesInSwagger = true))

    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger.json")))

    val jsonF = RRunner(service)
      .checkOk(r)
      .map(parse)
      .flatMap(_.map(_.pure[IO]).getOrElse(IO.raiseError(new Throwable("Expected Right value"))))

    val securityF = jsonF.map(json => (json \\ "security").flatMap(_.asArray).flatten)

    assertIOBoolean(
      securityF.map(
        _.contains(
          Json.obj(
            "bye" := List("hello")
          )
        )
      )
    )

    assertIOBoolean(
      securityF.map(
        _.contains(
          Json.obj(
            "hello" := List("bye")
          )
        )
      )
    )

    val summaryF = jsonF.map(json => (json \\ "summary").flatMap(_.asString))

    assertIOBoolean(summaryF.map(_.contains("Bye")))
    assertIOBoolean(summaryF.map(_.contains("Hello")))
  }

  test("SwaggerSupport should support for complex meta data") {
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

    val r = Request[IO](GET, Uri(path = Uri.Path.unsafeFromString("/swagger-test.json")))
    val jsonF = RRunner(service)
      .checkOk(r)
      .map(parse)
      .flatMap(_.map(_.pure[IO]).getOrElse(IO.raiseError(new Throwable("Expected Right value"))))

    for {
      cursor <- jsonF.map(_.hcursor)
      icn = cursor.downField("info").downField("contact").get[String]("name")
      h = cursor.get[String]("host")
      s = cursor.downField("schemes").as[List[String]]
      bp = cursor.get[String]("basePath")
      c = cursor.downField("consumes").downArray.as[String]
      p = cursor.downField("produces").downArray.as[String]
      sec1 = cursor.downField("security").downArray.downField("apiKey").as[List[String]]
      sec2 =
        cursor.downField("security").downArray.right.downField("vendor_jwk").downArray.as[String]
      t = cursor.downField("securityDefinitions").downField("api_key").get[String]("type")
      vi = cursor
        .downField("securityDefinitions")
        .downField("vendor_jwk")
        .get[String]("x-vendor-issuer")
      ve = cursor.downField("x-vendor-endpoints").get[String]("target")

      _ = assert(icn.fold(_ => false, _ == "Name"))
      _ = assert(h.fold(_ => false, _ == "www.test.com"))
      _ = assert(s.fold(_ => false, _ == List("http", "https")))
      _ = assert(bp.fold(_ => false, _ == "/v1"))
      _ = assert(c.fold(_ => false, _ == "application/json"))
      _ = assert(p.fold(_ => false, _ == "application/json"))
      _ = assert(sec2.fold(_ => false, _ == "admin"))
      _ = assert(t.fold(_ => false, _ == "apiKey"))
      _ = assert(vi.fold(_ => false, _ == "https://www.test.com/"))
      _ = assert(ve.fold(_ => false, _ == "298.0.0.1"))
      _ = assert(sec1.fold(_ => false, _ == List.empty[String]))
    } yield ()
  }
}
