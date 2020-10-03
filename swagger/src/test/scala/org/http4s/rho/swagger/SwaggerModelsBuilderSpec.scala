package org.http4s.rho
package swagger

import cats.effect.IO
import cats.syntax.all._
import cats.{Applicative, Monad}
import fs2.{Chunk, Stream}
import org.http4s.Method._
import org.http4s._
import org.http4s.headers._
import org.http4s.rho.bits.PathAST.{PathAnd, PathCapture}
import org.http4s.rho.bits._
import org.http4s.rho.io._
import org.http4s.rho.swagger.syntax.io._
import org.specs2.mutable.Specification

import scala.collection.compat.immutable.ArraySeq
import scala.collection.immutable.Seq
import scala.reflect._
import scala.reflect.runtime.universe._

object SwaggerModelsBuilderSpec {
  case class Foo(a: String, b: Int)
  case class Bar(c: Long, d: List[Foo])
  case class FooVal(str: String) extends AnyVal

  import org.json4s._
  import org.json4s.jackson.JsonMethods

  private implicit val format: DefaultFormats =
    DefaultFormats

  implicit def jsonParser[A : TypeTag : ClassTag]: StringParser[IO, A] = new StringParser[IO, A] with FailureResponseOps[IO] {
    override val typeTag: Option[TypeTag[A]] =
      implicitly[TypeTag[A]].some

    override def parse(s: String)(implicit F: Monad[IO]): ResultResponse[IO, A] = {
      Either.catchNonFatal(JsonMethods.parse(s).extract[A]) match {
        case Left(t) => badRequest[String](t.getMessage)
        case Right(t) => SuccessResponse(t)
      }
    }
  }
}

class SwaggerModelsBuilderSpec extends Specification {
  import SwaggerModelsBuilderSpec._
  import models._

  implicit def defaultCompiler: CompileRoutes[IO, RhoRoute.Tpe[IO]] =
    CompileRoutes.identityCompiler

  trait Renderable
  case class ModelA(name: String, color: Int) extends Renderable
  case class ModelB(name: String, id: Long) extends Renderable
  case class ModelC(name: String, shape: String) extends Renderable
  case class ModelMap(bar: String, baz: Map[String, Int]) extends Renderable

  val sb = new SwaggerModelsBuilder[IO](DefaultSwaggerFormats)
  val fooPath = GET / "foo"
  val barPath = GET / "bar"
  type OpSeq = Option[Seq[String]]

  "SwaggerModelsBuilder.collectQueryParams" should {

    "handle head request" in {
      val ra = HEAD / "foobar" |>> { "" }
      sb.collectPaths(ra)(Swagger()).get("/foobar").flatMap(_.head) must beSome[Operation]
    }

    "handle an action with one query parameter" in {
      val ra = fooPath +? param[Int]("id") |>> { (_: Int) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
      List(QueryParameter(`type` = "integer".some, name = "id".some, required = true))
    }

    "handle an action with one query parameter with description" in {
      val ra = fooPath +? paramD[Int]("id", "int id") |>> { (_: Int) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
        List(QueryParameter(`type` = "integer".some, name = "id".some, required = true, description = "int id".some))
    }

    "handle an action with one optional query parameter" in {
      val ra = fooPath +? param[Option[String]]("name") |>> { (_: Option[String]) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
      List(QueryParameter(`type` = "string".some, name = "name".some, required = false))
    }

    "handle an action with one optional seq query parameter" in {
      val ra = fooPath +? param[Option[Seq[String]]]("name") |>> { (_: Option[Seq[String]]) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
        List(
          QueryParameter(`type` = None, name = "name".some,
            items = Some(AbstractProperty(`type` = "string")),
            defaultValue = None, isArray = true, required = false)
        )
    }

    "handle an action with one optional seq query parameter using a type alias" in {
      val ra = fooPath +? param[OpSeq]("name") |>> { (_: OpSeq) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
        List(
          QueryParameter(`type` = None, name = "name".some,
            items = Some(AbstractProperty(`type` = "string")),
            defaultValue = None, isArray = true, required = false)
        )
    }

    "handle an action with one query parameter with default value" in {
      val ra = fooPath +? param[Int]("id", 6) |>> { (_: Int) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
      List(QueryParameter(`type` = "integer".some, name = "id".some, defaultValue = "6".some, required = false))
    }

    "handle an action with one query parameter with default value and description" in {
      val ra = fooPath +? paramD[Int]("id", 6, "id with default") |>> { (_: Int) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
        List(QueryParameter(`type` = "integer".some, name = "id".some, defaultValue = "6".some, required = false, description = "id with default".some))
    }

    "handle an action with two query parameters" in {
      val ra = fooPath +? param[Int]("id") & param[String]("str", "hello") |>> { (_: Int, _: String) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
      List(
        QueryParameter(`type` = "integer".some, name = "id".some,  required = true),
        QueryParameter(`type` = "string".some,  name = "str".some, defaultValue = "hello".some, required = false))
    }

    "handle an action with query or-structure" in {
      def orStr(str: String) = s"Optional if the following params are satisfied: [$str]".some

      val ra = fooPath +? (param[Int]("id") || param[Int]("id2")) |>> { (_: Int) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
      List(
        QueryParameter(`type` = "integer".some, name = "id".some,  description = orStr("id2"), required = true),
        QueryParameter(`type` = "integer".some, name = "id2".some, description = orStr("id"),  required = true))
    }

    "handle an action with one query parameter of complex data type" in {
       val ra = fooPath +? param[Foo]("foo") |>> { (_: Foo) => "" }

       sb.collectQueryParams(singleLinearRoute(ra)) must_==
       List(QueryParameter(`type` = "string".some, name = "foo".some, required = true))
    }

    "handle an action with one optional query parameter of complex data type" in {
       val ra = fooPath +? param[Option[Foo]]("foo") |>> { (_: Option[Foo]) => "" }

       sb.collectQueryParams(singleLinearRoute(ra)) must_==
       List(QueryParameter(`type` = "string".some, name = "foo".some, required = false))
    }

    "handle an action with one optional query parameter of complex (but AnyVal) data type" in {
       val ra = fooPath +? param[Option[FooVal]]("foo") |>> { (_: Option[FooVal]) => "" }

       sb.collectQueryParams(singleLinearRoute(ra)) must_==
       List(QueryParameter(`type` = "string".some, name = "foo".some, required = false))
    }

    "handle and action with two query parameters of complex data type" in {
      val ra = fooPath +? param[Foo]("foo") & param[Seq[Bar]]("bar", Nil) |>> { (_: Foo, _: Seq[Bar]) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
      List(
        QueryParameter(`type` = "string".some, name = "foo".some, required = true),
        QueryParameter(`type` = None, name = "bar".some, items = Some(AbstractProperty(`type` = "string")), defaultValue = "".some, isArray = true)
      )
    }

    "handle an action with query parameters of empty data types" in {
      val ra = fooPath +? param[Unit]("unit") & param[Void]("void") |>> { (_: Unit, _: Void) => "" }

      sb.collectQueryParams(singleLinearRoute(ra)) must_==
        List(
          QueryParameter(`type` = "string".some, name = "unit".some, required = true),
          QueryParameter(`type` = "string".some, name = "void".some, required = true),
        )
    }
  }

  "SwaggerModelsBuilder.collectHeaderParams" should {

    "handle an action with single header rule" in {
      val ra = fooPath >>> exists(`Content-Length`) |>> { () => "" }

      sb.collectHeaderParams(singleLinearRoute(ra)) must_==
      List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = true))
    }

    "handle an action with two header rules" in {
      val ra = fooPath >>> (exists(`Content-Length`) && exists(`Content-MD5`)) |>> { () => "" }

      sb.collectHeaderParams(singleLinearRoute(ra)) must_==
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, required = true),
        HeaderParameter(`type` = "string", name = "Content-MD5".some,    required = true))
    }

    "handle an action with or-structure header rules" in {
      def orStr(str: String) = s"Optional if the following headers are satisfied: [$str]".some

      val ra = fooPath >>> (exists(`Content-Length`) || exists(`Content-MD5`)) |>> { () => "" }

      sb.collectHeaderParams(singleLinearRoute(ra)) must_==
      List(
        HeaderParameter(`type` = "string", name = "Content-Length".some, description = orStr("Content-MD5"), required = true),
        HeaderParameter(`type` = "string", name = "Content-MD5".some,    description = orStr("Content-Length"), required = true))
    }

    "set required = false if there is a default value for the header" in {
      val ra = fooPath >>> captureOrElse(`Content-Length`)(`Content-Length`.unsafeFromLong(20)) |>> { (_: `Content-Length`) => "" }

      sb.collectHeaderParams(singleLinearRoute(ra)) must_==
        List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    }

    "set required = false if the header is optional" in {
      val ra = fooPath >>> captureOptionally(`Content-Length`) |>> { (_: Option[`Content-Length`]) => "" }

      sb.collectHeaderParams(singleLinearRoute(ra)) must_==
        List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    }

    "set required = false by default if there is a missingHeaderResult for the header" in {
      val ra = fooPath >>> captureMapR(`Content-Length`, Option(Ok("5")))(Right(_)) |>> { (_: `Content-Length`) => "" }

      sb.collectHeaderParams(singleLinearRoute(ra)) must_==
        List(HeaderParameter(`type` = "string", name = "Content-Length".some, required = false))
    }

  }

  "SwaggerModelsBuilder.collectBodyParams" should {

    "handle a required body parameter" in {
      val dec = new EntityDecoder[IO, Foo] {
        override def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, Foo] = ???
        override def consumes: Set[MediaRange] = Set.empty
        
      }
      val ra = fooPath.decoding(dec) |>> { _: Foo => "" }

      sb.collectBodyParams(singleLinearRoute(ra)) must beSome((p: BodyParameter) => p.required must_== true)
    }

    "handle an optional body parameter" in {
      val dec = new EntityDecoder[IO, Option[Foo]] {
        override def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, Option[Foo]] = ???
        override def consumes: Set[MediaRange] = Set.empty
      }
      val ra = fooPath.decoding(dec) |>> { _: Option[Foo] => "" }

      sb.collectBodyParams(singleLinearRoute(ra)) must beSome((p: BodyParameter) => p.required must_== false)
    }
  }

  "SwaggerModelsBuilder.mkOperation" should {

    "Get a route description" in {
      val ra = "foo" ** GET / "bar" |>> { () => "" }

      sb.mkOperation(singleLinearRoute(ra)).summary must_== "foo".some
    }

    "Get explicit route tag" in {
      val ra = "tag" @@ GET / "bar" |>> { () => "" }
      sb.mkOperation(singleLinearRoute(ra)).tags must_== List("tag")
    }

    "Get explicit route tags" in {
      val ra = List("tag1", "tag2") @@ GET / "bar" |>> { () => "" }

      sb.mkOperation(singleLinearRoute(ra)).tags must_== List("tag1", "tag2")
    }

    "Default to first segment for tags" in {
      val ra = GET / "foo" / "bar" |>> { () => "" }

      sb.mkOperation(singleLinearRoute(ra)).tags must_== List("foo")
    }

    "Default to / as a tag for routes without segments" in {
      val ra = GET |>> { () => "" }
      sb.mkOperation(singleLinearRoute(ra)).tags must_== List("/")
    }

    "Mix and match route descriptions and tags" in {
      val ra1 = "foo" ** List("tag1", "tag2") @@ GET / "bar" |>> { () => "" }
      val ra2 = List("tag1", "tag2") @@ ("foo" ** GET / "bar") |>> { () => "" }
      val ra3 = "foo" ** "tag" @@ GET / "bar" |>> { () => "" }
      val ra4 = "tag" @@ ("foo" ** GET / "bar") |>> { () => "" }

      sb.mkOperation(singleLinearRoute(ra1)) must_== sb.mkOperation(singleLinearRoute(ra2))
      sb.mkOperation(singleLinearRoute(ra3)) must_== sb.mkOperation(singleLinearRoute(ra4))
    }

    "Preserve explicit tags after prepending segments" in {
      val inner = List("tag1", "tag2") @@ GET / "bar" |>> { () => "" }
      val ra = "foo" /: inner

      sb.mkOperation(singleLinearRoute(ra)).tags must_== List("tag1", "tag2")
    }

    "Produce unique operation ids" in {
      val routes = Seq(
        "foo1" ** GET / "bar" |>> { () => "" },
        "foo2" ** GET / "bar" / pathVar[String]("name") |>> { _: String => "" },
        "foo3" ** GET / "bar" / pathVar[String]("name1") / pathVar[String]("name2") |>> { (_: String, _: String) => "" }
      )

      val operationIds = routes.foldLeft(Swagger())((s, r) => sb.mkSwagger(r)(s)).paths.values.toList.flatMap(_.get).flatMap(_.operationId)

      operationIds ==== List("getBar", "getBar-name", "getBar-name1-name2")
      }

    "Create Security Scope" in {
      val x:Map[String, List[String]]  = Map("oauth" -> List("admin"))

      val ra = x ^^ "foo1" ** GET / "bar" |>> { () => "" }

      sb.mkOperation(singleLinearRoute(ra)).security must_== List(x)
    }

    "Create Security Scopes" in {
      val x:Map[String, List[String]]  = Map("oauth" -> List("admin", "read"),"apiKey" -> List("read"))

      val ra = x ^^ "foo1" ** GET / "bar" |>> { () => "" }

      sb.mkOperation(singleLinearRoute(ra)).security must_== List(x)
    }

    "Handle collection params in body" in {
      val dec = new EntityDecoder[IO, List[String]] {
        override def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, List[String]] = ???
        override def consumes: Set[MediaRange] = Set.empty
      }

      val ra = POST / "foo" ^ dec |>> { (_: List[String]) => "" }

      sb.mkOperation(singleLinearRoute(ra)).parameters must_== List(
        BodyParameter(
          schema = ArrayModel(
            id = "scala.collection.immutable.List«java.lang.String»",
            id2 = "scala.collection.immutable.List«java.lang.String»",
            `type` = "array".some,
            items = RefProperty(ref = "String", title = "String".some).some).some,
          name = "body".some,
          description = "List«String»".some,
          required = true
        )
      )
    }
  }

  "SwaggerModelsBuilder.collectPaths" should {

    "find a simple path - GET" in {
      val ra = GET / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)(Swagger())

      paths must havePair("/foo" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple path - PUT" in {
      val ra = PUT / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)(Swagger())

      paths must havePair("/foo" -> Path(put = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple path - POST" in {
      val ra = POST / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)(Swagger())

      paths must havePair("/foo" -> Path(post = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple path - PATCH" in {
      val ra = PATCH / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)(Swagger())

      paths must havePair("/foo" -> Path(patch = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple path - OPTIONS" in {
      val ra = OPTIONS / "foo" |>> { () => "" }
      val paths = sb.collectPaths(ra)(Swagger())

      paths must havePair("/foo" -> Path(options = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple and-path" in {
      val ra = GET / "foo" / "bar" |>> { () => "" }
      val paths = sb.collectPaths(ra)(Swagger())

      paths must havePair("/foo/bar" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple or-path" in {
      val ra = GET / ("foo" || "bar") |>> { () => "" }

      val rFoo = GET / "foo" |>> { () => "" }
      val rBar = GET / "bar" |>> { () => "" }

      sb.collectPaths(ra)(Swagger()) must havePairs(
        "/foo" -> Path(get = sb.mkOperation(singleLinearRoute(rFoo)).some),
        "/bar" -> Path(get = sb.mkOperation(singleLinearRoute(rBar)).some))
    }

    "find a capture or-path" in {
      val ra = GET / (pathVar[Int]("foo") || pathVar[Int]("bar")) |>> { (_: Int) => "" }

      val rFoo = GET / pathVar[Int]("foo") |>> { (_: Int) => "" }
      val rBar = GET / pathVar[Int]("bar") |>> { (_: Int) => "" }

      sb.collectPaths(ra)(Swagger()) must havePairs(
        "/{foo}" -> Path(get = sb.mkOperation(singleLinearRoute(rFoo)).some),
        "/{bar}" -> Path(get = sb.mkOperation(singleLinearRoute(rBar)).some))
    }

    "find a simple path with a capture" in {
      val ra = GET / "foo" / pathVar[Int]("number") |>> { (_: Int) => "" }

      sb.collectPaths(ra)(Swagger()) must havePair(
        "/foo/{number}" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some))
    }

    "find a simple path with a capture with description" in {
      val ra = GET / pathVar[Int]("number", "int pathVar") |>> { (_: Int) => "" }

      sb.collectPaths(ra)(Swagger()) must havePair(
      "/{number}" -> Path(get = sb.mkOperation(singleLinearRoute(ra)).some))

      ra.path match {
        case PathAnd(_, p) =>
          p must beAnInstanceOf[PathCapture[IO]]
          p.asInstanceOf[PathCapture[IO]].description must_== "int pathVar".some

        case _ => false must_== true
      }
    }

    "maintain order of paths" in {
      def route(prefix: String) = {
        GET / prefix / pathVar[Int]("number", "int pathVar") |>> { (_: Int) => "" }
      }
      def result(prefix: String) =
        s"/${prefix}/{number}" -> Path(get = sb.mkOperation(singleLinearRoute(route(prefix))).some)

      def build(s: Swagger , r2: RhoRoute[IO, _]) =
        Swagger(paths = sb.collectPaths(r2)(s))

      val prefixRange = (0 to 10)
      val routes = prefixRange.map(i => s"p$i").map(route).toList
      val compiledRoutes = routes.foldLeft(Swagger())(build).paths.toList
      val results = prefixRange.map(i => s"p$i").map(result).toList

      compiledRoutes should_== results
    }

    "generate precise consumed media types" in {
      val dec = new EntityDecoder[IO, List[String]] {
        def consumes: Set[MediaRange] = Set(MediaType.application.json)
        def decode(m: Media[IO], strict: Boolean): DecodeResult[IO, List[String]] = ???
      }
      val ra = GET / "foo" ^ dec |>> { _: List[String] => "" }
      val op = sb.mkOperation(singleLinearRoute(ra))

      op.consumes must_== List("application/json")
    }
  }

  "SwaggerModelsBuilder.collectDefinitions" should {

    val prefix = "org.http4s.rho.swagger.SwaggerModelsBuilderSpec."
    val modelAFullName = prefix + "ModelA"
    val modelBFullName = prefix + "ModelB"
    val modelCFullName = prefix + "ModelC"

    "get available models" in {

      val ra = "testing models" ** GET / "models" |>> { () =>
        val a = 0
        a match {
          case 0 => Ok(ModelA("modela", 1))
          case 1 => NotFound(ModelB("modelb", 2))
          case 2 => PreconditionFailed(ModelC("modelc", "round"))
        }
      }

      sb.collectDefinitions(ra)(Swagger()) must havePairs(

        "ModelA" ->
          ModelImpl(
            id          = modelAFullName,
            id2         = "ModelA",
            description = "ModelA".some,
            `type`      = "object".some,
            properties  = Map(
              "name"  -> AbstractProperty("string", None, true),
              "color" -> AbstractProperty("integer", None, true, format = "int32".some))),

        "ModelB" ->
          ModelImpl(
            id          = modelBFullName,
            id2         = "ModelB",
            description = "ModelB".some,
            `type`      = "object".some,
            properties  = Map(
              "name" -> AbstractProperty("string", None, true),
              "id"   -> AbstractProperty("integer", None, true, format = "int64".some))),

        "ModelC" ->
          ModelImpl(
            id          = modelCFullName,
            id2         = "ModelC",
            description = "ModelC".some,
            `type`      = "object".some,
            properties  = Map(
              "name"  -> AbstractProperty("string", None, true),
              "shape" -> AbstractProperty("string", None, true)))
      )
    }

    "handle models with parameters" in {

      val ra = "testing models" ** GET / "models" |>> { () =>
        Ok((1, ModelA("modela", 1)))
      }

      sb.collectDefinitions(ra)(Swagger()) must havePairs(

        "Tuple2«Int,ModelA»" ->
          ModelImpl(
            id          = s"scala.Tuple2«scala.Int,$modelAFullName»",
            id2         = "Tuple2«Int,ModelA»",
            description = "Tuple2«Int,ModelA»".some,
            `type`      = "object".some,
            properties  = Map(
              "_1" -> AbstractProperty("integer", None, true, format = "int32".some),
              "_2" -> RefProperty(ref = "ModelA", required = true))))
    }

    "collect response of case class containing a map of primitive types" in {
      val ra = GET / "test" |>> { () => Ok(ModelMap("asdf", Map("foo"->1))) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = RefProperty("ModelMap",false,None,None,None).some
        )
      )
      sb.collectDefinitions(ra)(Swagger()) must havePair(
        "ModelMap" ->
          ModelImpl(
            id          = prefix + "ModelMap",
            id2         = "ModelMap",
            description = "ModelMap".some,
            `type`      = "object".some,
            properties  =  Map(
            "bar" -> AbstractProperty(`type` = "string", required = true),
            "baz" -> MapProperty(
              required = true,
              additionalProperties = AbstractProperty("integer", format = "int32".some)
            )
          )
        )
      )
    }
  }

  "SwaggerModelsBuilder.collectResponses" should {

    "collect an empty response with response code forbidding entity" in {
      val ra = GET / "test" |>> { () => NoContent.apply }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "204" -> Response(description = "No Content", schema = None))
    }

    "collect an empty response with response code allowing entity" in {
      val ra = GET / "test" |>> { () => Ok(()) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(description = "OK", schema = None))
    }

      "collect response of primitive types" in {
      val ra = GET / "test" |>> { () => Ok("") }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))
    }

    "collect response with file type" in {
      val ra = GET / "test" |>> { () => Ok(SwaggerFileResponse(CsvFile())) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "file").some))
    }

    "collect response of user-defined types" in {
      val ra = GET / "test" |>> { () => Ok(ModelA("", 0)) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(description = "OK", schema = RefProperty(ref = "ModelA").some))
    }

    "collect response of collection of primitive types" in {
      val ra = GET / "test" |>> { () => Ok(List("")) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = AbstractProperty(`type` = "string")).some))
    }

    "collect response of collection of unit" in {
      val ra = GET / "test" |>> { () => Ok(List(())) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Unit")).some))
    }

    "collect response of map of primitive types" in {
      val ra = GET / "test" |>> { () => Ok(Map("foo"->"bar")) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = MapProperty(additionalProperties = AbstractProperty(`type` = "string")).some))
    }

    "collect response of map of unit" in {
      val ra = GET / "test" |>> { () => Ok(Map(("foo", ()))) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = MapProperty(additionalProperties = RefProperty(ref = "Unit")).some))
    }

    "collect response of collection of user-defined types" in {
      val ra = GET / "test" |>> { () => Ok(List(ModelA("", 0))) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "ModelA")).some))
    }

    "collect response of tuples" in {
      val ra = GET / "test" |>> { () => Ok((0, ModelA("", 0))) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = RefProperty(ref = "Tuple2«Int,ModelA»").some))
    }

    "collect response of a collection of tuples" in {
      val ra = GET / "test" |>> { () => Ok(List((0, ModelA("", 0)))) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))
    }

    "collect response of a Stream of primitives" in {
      val ra1 = GET / "test" |>> { () => Ok(Stream.eval(IO.pure(""))) }
      val ra2 = GET / "test" |>> { () => Ok(Stream.emit("").covary[IO]) }

      sb.collectResponses(singleLinearRoute(ra1)) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))

      sb.collectResponses(singleLinearRoute(ra2)) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))
    }

    "collect response of a Stream of non-primitives" in {
      val ra1 = GET / "test" |>> { () => Ok(Stream.eval(IO.pure(List((0, ModelA("", 0)))))) }
      val ra2 = GET / "test" |>> { () => Ok(Stream.emit(List((0, ModelA("", 0)))).covary[IO]) }

      sb.collectResponses(singleLinearRoute(ra1)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))

      sb.collectResponses(singleLinearRoute(ra2)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))
    }

    "collect response of an IO of a primitive" in {
      val ra = GET / "test" |>> { () => Ok("") }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(description = "OK", schema = AbstractProperty(`type` = "string").some))
    }

    "collect response of an IO of a non-primitive" in {
      val ra = GET / "test" |>> { () => Ok(List((0, ModelA("", 0)))) }

      sb.collectResponses(singleLinearRoute(ra)) must havePair(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some))
    }

    "collect multiple responses" in {
      val ra = GET / "test" / pathVar[Int] |>> { (i: Int) =>

        i match {
          case 0 => Ok(List((0, ModelA("A", 0))))
          case _ => Unauthorized("Unauthorized")
        }
      }

      sb.collectResponses(singleLinearRoute(ra)) must havePairs(
        "200" -> Response(
          description = "OK",
          schema      = ArrayProperty(items = RefProperty(ref = "Tuple2«Int,ModelA»")).some),
        "401" -> Response(
          description = "Unauthorized",
          schema      = AbstractProperty(`type` = "string").some))
    }
  }

  def singleLinearRoute(rr: RhoRoute[IO, _]): sb.LinearRoute =
    sb.linearizeRoute(rr) match {
      case Seq(lr) => lr
      case other => throw new Exception(s"Expected $rr\n to linearize to exactly one LinearRoute. Instead got:\n${other.mkString("\n")}")
    }

  implicit def renderableEncoder[F[_], T <: Renderable]: EntityEncoder[F, T] =
    EntityEncoder
      .stringEncoder[F](Charset.`UTF-8`)
      .contramap { _: T => "" }
      .withContentType(`Content-Type`(MediaType.application.json, Charset.`UTF-8`))

  implicit def tuple2Encoder[F[_], T <: Renderable]: EntityEncoder[F, (Int, T)] =
    EntityEncoder
      .stringEncoder[F](Charset.`UTF-8`)
      .contramap { _: (Int, T) => "" }
      .withContentType(`Content-Type`(MediaType.application.json, Charset.`UTF-8`))

  implicit def listEntityEncoder[F[_]: Applicative, A]: EntityEncoder[F, List[A]] =
    EntityEncoder.simple[F, List[A]]()(_ => Chunk.bytes("A".getBytes))

  implicit def mapEntityEncoder[F[_]: Applicative, A, B]: EntityEncoder[F, Map[A,B]] =
    EntityEncoder.simple[F, Map[A,B]]()(_ => Chunk.bytes("A".getBytes))

  case class CsvFile()

  object CsvFile {
    implicit def entityEncoderCsvFile: EntityEncoder[IO, CsvFile] =
      EntityEncoder.encodeBy[IO, CsvFile](`Content-Type`(MediaType.text.csv, Some(Charset.`UTF-8`))) { _: CsvFile =>
        val bv = "file content".getBytes(Charset.`UTF-8`.nioCharset)
        org.http4s.Entity(Stream.emits(ArraySeq.unsafeWrapArray(bv)), Some(bv.length))
      }
  }
}
