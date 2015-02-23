package org.http4s.rho
package swagger

import org.http4s._
import org.http4s.Method._
import org.http4s.Header._
import org.http4s.rho.bits.QueryAST.EmptyQuery

import org.specs2.mutable.Specification

import scalaz._, Scalaz._

class SwaggerModelsBuilderSpec extends Specification {
  import models._, DummyCompiler.compilerInstance

  object dummy {
    sealed abstract class Renderable
    case class ModelA(name: String, color: Int) extends Renderable
    case class ModelB(name: String, id: Long) extends Renderable
    case class ModelC(name: String, shape: String) extends Renderable
  }

  val sb = new SwaggerModelsBuilder(DefaultSwaggerFormats)
  val fooPath = GET / "foo"
  val barPath = GET / "bar"

  "SwaggerModelsBuilder.collectQueryParams" should {

    "handle an action with one query parameter" in {
      val ra = fooPath +? param[Int]("id") |>> { (i: Int) => "" }

      sb.collectQueryParams(ra) must_==
      List(QueryParameter(`type` = "integer", name = "id".some, required = true))
    }

    "handle an action with one query parameter with default value" in {
      val ra = fooPath +? param[Int]("id", 6) |>> { (i: Int) => "" }

      sb.collectQueryParams(ra) must_==
      List(QueryParameter(`type` = "integer", name = "id".some, defaultValue = "6".some, required = false))
    }

    "handle an action with two query parameters" in {
      val ra = fooPath +? param[Int]("id") & param[String]("str", "hello") |>> { (i: Int, s: String) => "" }

      sb.collectQueryParams(ra) must_==
      List(
        QueryParameter(`type` = "integer", name = "id".some,  required = true),
        QueryParameter(`type` = "string",  name = "str".some, defaultValue = "hello".some, required = false))
    }

    "handle an action with query or-structure" in {

      def orStr(str: String) = s"Optional if the following params are satisfied: [$str]".some

      val ra = fooPath +? (param[Int]("id") || param[Int]("id2")) |>> { (i: Int) => "" }

      sb.collectQueryParams(ra) must_==
      List(
        QueryParameter(`type` = "integer", name = "id".some,  description = orStr("id2"), required = true),
        QueryParameter(`type` = "integer", name = "id2".some, description = orStr("id"),  required = true))
    }
  }

  "SwaggerModelsBuilder.collectHeaderParams" should {

    "handle an action with single header rule" in {
      val ra = fooPath >>> require(`Content-Length`) |>> { () => "" }

      sb.collectHeaderParams(ra) must_==
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = true))
    }

    "handle an action with two header rules" in {
      val ra = fooPath >>> (require(`Content-Length`) && require(`Content-MD5`)) |>> { () => "" }

      sb.collectHeaderParams(ra) must_==
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, required = true),
        HeaderParameter(`type` = "string", name = "Content-MD5".some,    required = true))
    }

    "handle an action with or-structure header rules" in {

      def orStr(str: String) = s"Optional if the following headers are satisfied: [$str]".some

      val ra = fooPath >>> (require(`Content-Length`) || require(`Content-MD5`)) |>> { () => "" }

      sb.collectHeaderParams(ra) must_==
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, description = orStr("Content-MD5"), required = true),
        HeaderParameter(`type` = "string", name = "Content-MD5".some,    description = orStr("Content-Length"), required = true))
    }
  }

  "SwaggerModelsBuilder.mkOperation" should {

    "Get a route description" in {
      val ra = "foo" ** GET / "bar" |>> { () => "" }

      sb.mkOperation("/foo", ra).summary must_== "foo".some
    }
  }

  "SwaggerModelsBuilder.collectPaths" should {

    "find a simple path - GET" in {
      val ra = GET / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)
      
      paths must havePair("/foo" -> Path(get = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - PUT" in {
      val ra = PUT / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)
      
      paths must havePair("/foo" -> Path(put = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - POST" in {
      val ra = POST / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)
      
      paths must havePair("/foo" -> Path(post = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - PATCH" in {
      val ra = PATCH / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)
      
      paths must havePair("/foo" -> Path(patch = sb.mkOperation("/foo", ra).some))
    }

    "find a simple path - OPTIONS" in {
      val ra = OPTIONS / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)
      
      paths must havePair("/foo" -> Path(options = sb.mkOperation("/foo", ra).some))
    }

    "find a simple and-path" in {
      val ra = GET / "foo" / "bar" |>> { () => "" }
      val paths = sb.collectPaths(ra)
      
      paths must havePair("/foo/bar" -> Path(get = sb.mkOperation("/foo/bar", ra).some))
    }

    "find a simple or-path" in {
      val ra = GET / ("foo" || "bar") |>> { () => "" }

      sb.collectPaths(ra) must havePairs(
        "/foo" -> Path(get = sb.mkOperation("/foo", ra).some),
        "/bar" -> Path(get = sb.mkOperation("/bar", ra).some))
    }

    "find a capture or-path" in {
      val ra = GET / (pathVar[Int]("foo") || pathVar[Int]("bar")) |>> { (i: Int) => "" }

      sb.collectPaths(ra) must havePairs(
        "/{foo}" -> Path(get = sb.mkOperation("{foo}", ra).some),
        "/{bar}" -> Path(get = sb.mkOperation("{bar}", ra).some))
    }

    "find a simple path with a capture" in {
      val ra = GET / "foo" / pathVar[Int]("number") |>> { (i: Int) => "" }

      sb.collectPaths(ra) must havePair(
        "/foo/{number}" -> Path(get = sb.mkOperation("foo/{number}", ra).some))
    }
  }

  "SwaggerModelsBuilder.collectDefinitions" should {

    "get available models" in {
      import dummy._
      import org.http4s.rho.bits.ResponseGeneratorInstances._

      val prefix = "org.http4s.rho.swagger.SwaggerModelsBuilderSpec.dummy"
      val modelAFullName = prefix + ".ModelA"
      val modelBFullName = prefix + ".ModelB"
      val modelCFullName = prefix + ".ModelC"

      implicit def renderableEncoder[T <: Renderable]: EntityEncoder[T] =
        EntityEncoder
          .stringEncoder(Charset.`UTF-8`)
          .contramap { r: T => "" }
          .withContentType(`Content-Type`(MediaType.`application/json`, Charset.`UTF-8`))

      val ra = "testing models" ** GET / "models" |>> { () =>
        val a = 0
        a match {
          case 0 => Ok(ModelA("modela", 1))
          case 1 => NotFound(ModelB("modelb", 2))
          case 2 => PreconditionFailed(ModelC("modelc", "round"))
        }
      }      

      sb.collectDefinitions(ra) must havePairs(

        modelAFullName ->
          ModelImpl(
            description = modelAFullName,
            properties  = Map(
              "name"  -> AbstractProperty("string", true),
              "color" -> AbstractProperty("integer", true, format = "int32".some))),

        modelBFullName ->
          ModelImpl(
            description = modelBFullName,
            properties  = Map(
              "name" -> AbstractProperty("string", true),
              "id"   -> AbstractProperty("integer", true, format = "int64".some))),

        modelCFullName ->
          ModelImpl(
            description = modelCFullName,
            properties  = Map(
              "name"  -> AbstractProperty("string", true),
              "shape" -> AbstractProperty("string", true)))
      )
    }
  }
}
