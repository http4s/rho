# Documenting Http4s web services with rho

Rho is a library for documenting [http4s](https://http4s.org/) web APIs.

Note: To get the generated swagger.json, you need to visit: `{https|http}://host:port/{base service path if any}/swagger.json`

Sample:

```scala
import org.http4s.rho.RhoRoutes

val api = new RhoRoutes[IO] {
    GET / "somePath" / pathVar[Int]("someInt", "parameter description") +? paramD[String]("name", "parameter description") |>> {
      (someInt: Int, name: String) => Ok("result")
    }
}
```

So we start by creating a new RhoService which will be converted to an HttpService later. This RhoService will contain our api endpoints. Every api endpoint starts (optionally, for Swagger) with a description for that endpoint and then the implementation. We describe the HTTP method (GET/POST/DELETE etc) and then an implementation for the url endpoint.

You can specify and capture path or query parameters in your path. You specify a name and description (optional) for the variables. The available types that you can capture are:
- all primitive types
- `java.util.Date` (yyyy-MM-dd)
- `java.util.UUID`
- `java.time.Instant` (yyyy-MM-ddThh:mm:ssZ)

Checkout these methods:
- `pathVar` functions for capturing path variables
- `param` functions for capturing query parameters
- `capture` functions for capturing headers

Think of `|>>` as a function that "registers" new route matcher and associated action in RhoService.
Route matcher is at lefthand-side (represented as `TypedBuilder`) and action at righthand-side is a function returning HTTP response.
The arguments for action appear exactly as they do in the URL spec (route matcher).
So in the sample above, `someInt` is first then `name` and so on. Optionally it can accept whole `org.http4s.Request` as first argument.

Apart from plain URL parameters and path, you can insert header captures and body decoders for your requests, an example:

```scala
import org.http4s.rho.RhoRoutes

object Auth extends org.http4s.rho.AuthedContext[IO, AuthInfo]

val api = new RhoRoutes[IO] {
    import org.http4s.rho.swagger.syntax.io._
    
    "Description of api endpoint" **      // Description is optional and specific to Swagger
    POST / "somePath" / pathVar[Int]("someInt", "parameter description") >>> Auth.auth ^ jsonOf[IO, T] |>> {
      (someInt: Int, au: AuthInfo, body: T) => Ok("result")
    }
}
```

`Auth.auth()` captures the authentication information.
In order to provide authentication information:
1. `AuthedContext` should be transformed into `HttpService`
2. `AuthMiddleware[IO, AuthInfo]` must wrap above service

```scala
val authInfoMiddleware: org.http4s.server.AuthMiddleware[IO, AuthInfo] = ???  // Standard http4s AuthMiddleware
val authApi = Auth.toService(api.toService())                                 // 1
val service: org.http4s.HttpService[IO] = authInfoMiddleware.apply(authApi)   // 2
```

Also, in example above, we are specifying that the body is a json object for some `T`.
Anything that has a `EntityDecoder` can be specified as a body decoder. In the example above `jsonOf` is an `EntityDecoder` provided by `circe`. Included decoders in Rho are:
- binary
- binaryChunk
- byteArrayDecoder
- text
- charArrayDecoder
- binFile
- textFile
- multipart
- void

This will all work automatically for the most part and that's all you need. But sometimes the models for apis are too big and complex and you need documentation on every field in your model. This type of information is not captured in Rho right now and we have to provide specifications for those manually. For that we can make use of `withCustomSerializer` in `SwaggerSupport`.

```scala
case class MyClass(name: String, description: String, someBool: Boolean, tags: List[String], someObj: Map[String, String])
//or
case class MyClass(name: String, description: String, someBool: Boolean, tags: List[String], someObj: JsonObject)

import org.http4s.rho.swagger.models._

val myClassModel: Set[Model] = Set(
    ModelImpl(
      id = "MyClass",
      id2 = "MyClass",
      `type` = "object".some,
      description = "MyClass".some,
      name = "MyClass".some,
      properties = Map(
        "name" -> StringProperty(
          required = true,
          description = "name of MyClass".some,
          enums = Set()
        ),
        "category" -> StringProperty(
          required = true,
          description = "enum of category".some,
          enums = Set("A", "B", "C")
        ),
        "someBool" -> AbstractProperty(
          required = true,
          description =
            "boolean".some,
          format = "bool".some
        ),
        "tags" -> ArrayProperty(
          required = true,
          description = "some tags".some,
          items = StringProperty(required = true,
                                 enums = Set(),
                                 description = "tag name".some),
          uniqueItems = true
        ),
        "someObj" -> ObjectProperty(
          required = true,
          description = "some object".some
        )
      ),
      example =
        """{"name" : "MyClass1",  "category" : "A", "tags" : ["tag1", "tag2"], "someBool" : false, "someObj": {}}""".some
    )
  )

import org.http4s.rho.RhoMiddleware
import org.http4s.rho.swagger.syntax.{io => ioSwagger}

// Create a middleware that will transform RhoService into HttpService with attached Swagger definition
val swaggerMiddleware: RhoMiddleware[IO] = ioSwagger.createRhoMiddleware(
  swaggerFormats = DefaultSwaggerFormats
    .withSerializers(typeOf[MyClass], myClassModel)
    .withSerializers(...),
    apiInfo = Info(
    title = "My API",
    version = "1.0.0",
    description = Some("functional because who hates sleep?")
  ),
  basePath = "/v1".some,
  schemes = List(Scheme.HTTPS),
  security = List(SecurityRequirement("bearer", List())),
  securityDefinitions = Map(
    "bearer" -> ApiKeyAuthDefinition("Authorization", In.HEADER)
  ))
  
// Create http4s HttpService
val httpService = api.toRoute(swaggerMiddleware)
```

The example above also shows how to provide basic api info and base path and security specification.
By declaring security here we get option to add api key/token in the Swagger UI.

To get the generated swagger.json for this example, you would visit: `https://host:port/v1/swagger.json`
